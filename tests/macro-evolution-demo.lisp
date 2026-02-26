; macro-evolution-demo.lisp – Selbstmodifizierender Code
; Autor: Claude
; CoAutor: Gerhard Quell
; Erstellt: 20260226
;
; Demonstriert Homoikonizität: Code generiert Code.
; Zeigt defmacro, quasiquote, unquote-splice, gensym, eval
; und wie Makros sich selbst optimieren können.

(load "tests/demo-utils.lisp")
(load "tests/macro-utils.lisp")

(print-header "Macro Evolution Demo")
(println "Demonstration: Code als Daten – Daten als Code")
(println "")

;; ============================================================================
;; Teil 1: Grundlagen der Code-Generierung
;; ============================================================================

(print-header "Teil 1: Quasiquote in Aktion")

;; Einfache Code-Generierung
(define x 10)
(define y 20)

(println "x = 10, y = 20")
(println "`(list ,x ,y)       = " `(list ,x ,y))
(println "`(list ',x ',y)     = " `(list ',x ',y))
(println "`(list x y)         = " `(list x y))
(println "")

;; Unquote-Splice demonstrieren
(define nums '(1 2 3))
(println "nums = '(1 2 3)")
(println "`(list ,nums)       = " `(list ,nums))
(println "`(list ,@nums)      = " `(list ,@nums))
(println "")

;; ============================================================================
;; Teil 2: Selbstmodifizierende Funktionen
;; ============================================================================

(print-header "Teil 2: Selbstmodifizierender Code")

;; Ein Makro, das optimisierte Code-Varianten generiert
(defmacro optimize-for (pattern . body)
  "Generiert optimierten Code basierend auf einem Muster"
  (cond
    ;; Pattern: (+ x 0) -> x
    ((and (eq (car pattern) '+)
          (equal? (caddr pattern) 0))
     `(begin
        (println "  [Optimierung: (+ x 0) -> x]")
        ,(cadr pattern)))

    ;; Pattern: (* x 1) -> x
    ((and (eq (car pattern) '*)
          (equal? (caddr pattern) 1))
     `(begin
        (println "  [Optimierung: (* x 1) -> x]")
        ,(cadr pattern)))

    ;; Pattern: (* x 0) -> 0
    ((and (eq (car pattern) '*)
          (equal? (caddr pattern) 0))
     `(begin
        (println "  [Optimierung: (* x 0) -> 0]")
        0))

    ;; Keine Optimierung bekannt
    (else
     `(begin
        (println "  [Keine Optimierung bekannt]")
        ,@body))))

(println "Optimierungs-Makro:")
(println "  (optimize-for (+ x 0) (+ 5 0))")
(println "  Ergebnis: " (optimize-for (+ x 0) (+ 5 0)))
(println "")

(println "  (optimize-for (* x 1) (* 5 1))")
(println "  Ergebnis: " (optimize-for (* x 1) (* 5 1)))
(println "")

(println "  (optimize-for (* x 0) (* 5 0))")
(println "  Ergebnis: " (optimize-for (* x 0) (* 5 0)))
(println "")

;; ============================================================================
;; Teil 3: Meta-Makros (Makros die Makros schreiben)
;; ============================================================================

(print-header "Teil 3: Meta-Makros")

;; Ein Makro, das ein Makro generiert
(defmacro def-optimized (name params pattern . body)
  "Definiert eine Funktion mit eingebauter Optimierung"
  `(defmacro ,name ,params
     (optimize-for ,pattern ,@body)))

;; Anwendung: definiere eine optimierte Add-Funktion
(println "Definiere 'optimized-add' via Meta-Makro:")
(def-optimized optimized-add (a b) (+ a 0)
  (+ a b))

(println "Verwendung:")
(println "  (optimized-add 5 0) = " (optimized-add 5 0))
(println "  (optimized-add 5 3) = " (optimized-add 5 3))
(println "")

;; ============================================================================
;; Teil 4: Code-Analyse und Transformation
;; ============================================================================

(print-header "Teil 4: Code-Analyse und Transformation")

;; Ein Makro, das Code-Statistiken sammelt
(defmacro with-stats (expr)
  "Wrappt Ausdruck mit Statistik-Sammlung"
  (let ((result-sym (gensym))
        (start-sym (gensym)))
    `(let ((,start-sym (memstats)))
       (println "  [Statistik-Start]")
       (let ((,result-sym ,expr))
         (println "  [Statistik-Ende]")
         (println "    Ausdruck: ",(quote ,expr))
         (println "    Ergebnis: ",result-sym)
         ,result-sym))))

(println "Code mit Statistik-Tracking:")
(with-stats (+ (* 2 3) (* 4 5)))
(println "")

;; ============================================================================
;; Teil 5: Dynamische Code-Generierung mit eval
;; ============================================================================

(print-header "Teil 5: Dynamische Code-Generierung")

;; Generiere einen Filter-Ausdruck basierend auf Bedingungen
(defun generate-filter (pred-sym lst-sym conditions)
  "Generiert einen Filter-Ausdruck als Liste"
  `(filter (lambda (,pred-sym)
             (and ,@conditions))
           ,lst-sym))

(println "Dynamische Filter-Generierung:")
(println "  Bedingungen: ( (> x 5) ( < x 20) )")

(define filter-expr
  (generate-filter 'x 'numbers '((> x 5) (< x 20))))

(println "  Generierter Code: " filter-expr)
(println "")

;; Ausführen des generierten Codes
(define numbers '(1 3 7 10 15 25 30))
(println "  Daten: " numbers)
(println "  Ergebnis (eval): " (eval filter-expr))
(println "")

;; ============================================================================
;; Teil 6: Sich selbst verbessernde Funktion
;; ============================================================================

(print-header "Teil 6: Self-Improving Function")

;; Demonstriert: KI generiert Code, GoLisp führt ihn aus

(define *original-code* '(+ x x))
(define *improved-code* '(* 2 x))

(println "Ursprüngliche Berechnung (Code als Daten):")
(println "  " *original-code*)
(println "")

(println "KI-generierte Optimierung:")
(println "  " *improved-code*)
(println "")

;; Evaluiere beide Versionen mit x=10
(define x 10)
(println "Vergleich mit x = 10:")
(println "  Original:  " (eval *original-code*))
(println "  Optimiert: " (eval *improved-code*))
(println "")

(println "Code-Evolution demonstriert: Programme können ihren")
(println "eigenen Code analysieren, optimieren und ausführen!")
(println "")

;; ============================================================================
;; Teil 7: Hygiene mit gensym
;; ============================================================================

(print-header "Teil 7: Makro-Hygiene mit gensym")

;; Nicht-hygienisches Makro (problematisch)
(defmacro problematic-swap (a b)
  "Nicht-hygienisch – konflikt mit temp-Variable möglich"
  `(let ((temp ,a))
     (set! ,a ,b)
     (set! ,b temp)))

;; Hygienisches Makro mit gensym
(defmacro hygienic-swap (a b)
  "Hygienisch – temp ist immer eindeutig"
  (let ((temp (gensym)))
    `(let ((,temp ,a))
       (set! ,a ,b)
       (set! ,b ,temp))))

(println "Test: (let ((temp 100) (x 10) (y 20))")
(println "        (hygienic-swap x y)")
(println "        (list temp x y))")

(define swap-result
  (let ((temp 100)
        (x 10)
        (y 20))
    (hygienic-swap x y)
    (list temp x y)))

(println "  Ergebnis: " swap-result)
(println "  (temp bleibt 100, x=20, y=10 – keine Kollision)")
(println "")

;; ============================================================================
;; Zusammenfassung
;; ============================================================================

(print-header "Zusammenfassung")

(println "Diese Demo hat gezeigt:")
(println "")
(println "  1. Quasiquote erlaubt Code-Templates")
(println "  2. Makros können Code optimieren")
(println "  3. Meta-Makros schreiben andere Makros")
(println "  4. eval führt dynamisch generierten Code aus")
(println "  5. gensym garantiert Hygiene")
(println "  6. Code kann sich selbst verbessern (KI-Assisted)")
(println "")
(println "GoLisp's Homoikonizität + KI-Anbindung =")
(println "  Ein selbsterweiterndes System, das sich durch KI-Calls")
(println "  selbst vervollständigen kann!")
