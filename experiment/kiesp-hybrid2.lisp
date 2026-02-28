;;; kiesp-hybrid.lisp - Hybrid-Ansatz (Dictionary + fraktale Verben)

(load "experiment/kiesp-stack.lisp")
(load "experiment/kiesp-sexp.lisp")
(load "experiment/kiesp-dict.lisp")

(define kiesp-context 'unknown)

(defun kiesp-detect-context (text)
  "Erkennt Kontexttyp des Textes"
  (set! kiesp-context
    (cond
      ((kiesp-code-pattern? text) 'code)
      ((kiesp-narrative-pattern? text) 'narrative)
      ((kiesp-data-pattern? text) 'data)
      (else 'general))))
  kiesp-context)

(defun kiesp-code-pattern? (text)
  "Prueft auf Code-Muster"
  (or (kiesp-contains? text "(")
      (kiesp-contains? text "defun")
      (kiesp-contains? text "lambda")
      (kiesp-contains? text "define")))

(defun kiesp-narrative-pattern? (text)
  "Prueft auf narrative Muster"
  (or (kiesp-contains? text "the ")
      (kiesp-contains? text " and ")
      (kiesp-contains? text ". ")))

(defun kiesp-data-pattern? (text)
  "Prueft auf Daten-Muster"
  (or (kiesp-contains? text "{")
      (kiesp-contains? text "[")
      (kiesp-contains? text ": ")))

(defun kiesp-contains? (text substr)
  "Prueft ob String Teilstring enthaelt"
  (kiesp-contains-helper text substr 0))

(defun kiesp-contains-helper (text substr start)
  "Hilfsfunktion fuer contains"
  (if (kiesp-greater (+ start (string-length substr)) (string-length text))
      '()
      (if (string=? (substring text start (+ start (string-length substr))) substr)
          't
          (kiesp-contains-helper text substr (+ start 1)))))

;;; Encoder-Funktionen

(defun kiesp-encode (text)
  "Waehlt besten Encoder basierend auf Kontext"
  (kiesp-detect-context text)
  (case kiesp-context
    ('code (kiesp-encode-code text))
    ('narrative (kiesp-encode-narrative text))
    ('data (kiesp-encode-data text))
    (else (kiesp-encode-narrative text))))

(defun kiesp-encode-code (text)
  "Kodiert Code mit S-Expr-Kompression"
  (let ((expr (read (string-append "(" text ")"))))
    (list
      (cons 'context 'code)
      (cons 'method 'sexp)
      (cons 'compressed (kiesp-compress expr))
      (cons 'original-tokens (length (flatten expr))))))

(defun kiesp-encode-narrative (text)
  "Kodiert Narrativ mit Dictionary"
  (let ((dict-result (kiesp-generate-dict text 20)))
    (list
      (cons 'context 'narrative)
      (cons 'method 'dictionary)
      (cons 'compressed (kiesp-encode-dict text))
      (cons 'dictionary (car dict-result))
      (cons 'stats (kiesp-stats-dict text (kiesp-encode-dict text))))))

(defun kiesp-encode-data (text)
  "Kodiert Daten mit Stack-Fraktalen"
  (let ((tokens (kiesp-tokenize text)))
    (list
      (cons 'context 'data)
      (cons 'method 'stack)
      (cons 'compressed 'stack-mode)
      (cons 'original-tokens (length tokens)))))

;;; Effizienz-Messung

(defun kiesp-measure (original encoded)
  "Misst Effizienz einer Kodierung"
  (let ((orig-size (kiesp-size original))
        (enc-size (kiesp-size encoded)))
    (list
      (cons 'original-bytes orig-size)
      (cons 'encoded-bytes enc-size)
      (cons 'ratio (/ (* 100.0 enc-size) orig-size))
      (cons 'savings (* 100.0 (- 1.0 (/ enc-size orig-size))))
      (cons 'efficiency (kiesp-efficiency-score orig-size enc-size)))))

(defun kiesp-size (x)
  "Schaetzt Groesse eines Objekts in Bytes"
  (cond
    ((null? x) 2)
    ((string? x) (string-length x))
    ((number? x) 8)
    ((atom? x) (string-length (symbol->string x)))
    (else (+ 2 (kiesp-size (car x)) (kiesp-size (cdr x))))))

(defun kiesp-efficiency-score (orig enc)
  "Berechnet Effizienz-Score (0-100)"
  (let ((compression (- 1.0 (/ enc orig)))
        (overhead (if (kiesp-less enc orig) 0.0 (- 1.0 (/ orig enc)))))
    (* 50.0 (+ compression (- 1.0 overhead)))))

;;; Benchmarking

(define kiesp-benchmark-results '())

(defun kiesp-benchmark (name text)
  "Fuehrt Benchmark fuer einen Text durch"
  (let ((start (memstats))
        (encoded (kiesp-encode text))
        (end (memstats)))
    (let ((result (list
                    (cons 'name name)
                    (cons 'context kiesp-context)
                    (cons 'encoded encoded)
                    (cons 'measurement (kiesp-measure text encoded))
                    (cons 'memory-used (- (cdr (assoc 'heapalloc end))
                                          (cdr (assoc 'heapalloc start)))))))
      (set! kiesp-benchmark-results (cons result kiesp-benchmark-results))
      result)))

(defun kiesp-report ()
  "Erzeugt Benchmark-Report"
  (println "=== KIESP Hybrid Benchmark Report ===")
  (kiesp-print-results kiesp-benchmark-results))

(defun kiesp-print-results (results)
  "Druckt Ergebnisse"
  (if (null? results)
      (println "=== Ende Report ===")
      (begin
        (println "Test: " (cdr (assoc 'name (car results))))
        (println "  Context: " (cdr (assoc 'context (car results))))
        (println "  Stats:   " (cdr (assoc 'measurement (car results))))
        (kiesp-print-results (cdr results)))))

;;; Hilfsfunktionen statt Operatoren die konflikten koennten

(defun kiesp-greater (a b) (> a b))
(defun kiesp-less (a b) (< a b))

;;; Test-Texte

(define kiesp-test-code
  "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))")

(define kiesp-test-narrative
  "The quick brown fox jumps over the lazy dog and runs away into the forest")

(define kiesp-test-data
  "name: John age: 30 city: New York active: true")

(println "=== KIESP Hybrid geladen ===")
(println "Funktionen: kiesp-detect-context kiesp-encode kiesp-measure kiesp-benchmark")
(println "Test-Texte: kiesp-test-code kiesp-test-narrative kiesp-test-data")
