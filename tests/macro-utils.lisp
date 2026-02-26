; macro-utils.lisp – Wiederverwendbare Makro-Hilfsmittel
; Autor: Claude
; CoAutor: Gerhard Quell
; Erstellt: 20260226
;
; Sammlung nützlicher Makros und Makro-Helper für GoLisp.
; Demonstriert: defmacro, quasiquote, unquote, gensym

;; Hilfsfunktion für Symbol-Konkatenation
(defun concat-symbols (sym1 sym2)
  "Verbindet zwei Symbole zu einem neuen Symbol"
  (string->symbol
   (string-append (symbol->string sym1)
                  "-"
                  (string-append (symbol->string sym2)))))

;; String->Symbol und zurück
(defun string->symbol (str)
  "Konvertiert String zu Symbol via eval/read-Zirkel"
  (read (string-append "'" str)))

(defun symbol->string (sym)
  "Konvertiert Symbol zu String"
  (if (atom sym)
      (string-append "" sym)
      "complex"))

;; ---------------------------------------------------------------------------
;; Grundlegende Kontrollfluss-Makros
;; ---------------------------------------------------------------------------

(defmacro unless (condition . body)
  "Führt body aus wenn condition falsch ist"
  `(if ,condition
       nil
       (begin ,@body)))

(defmacro when (condition . body)
  "Führt body aus wenn condition wahr ist"
  `(if ,condition
       (begin ,@body)
       nil))

(defmacro cond clauses
  "Mehrweg-Verzweigung: (cond (test expr)... [(else expr)])"
  (if (null? clauses)
      nil
      (if (eq (caar clauses) 'else)
          `(begin ,@(cdar clauses))
          `(if ,(caar clauses)
               (begin ,@(cdar clauses))
               (cond ,@(cdr clauses))))))

;; ---------------------------------------------------------------------------
;; Variablen-Bindung
;; ---------------------------------------------------------------------------

(defmacro let* (bindings . body)
  "Sequentielles let – jede Bindung sieht vorherige"
  (if (null? bindings)
      `(let () ,@body)
      `(let (,(car bindings))
         (let* ,(cdr bindings) ,@body))))

;; ---------------------------------------------------------------------------
;; Schleifen-Makros
;; ---------------------------------------------------------------------------

(defmacro dotimes (spec . body)
  "Wiederholt body n mal: (dotimes (i n) ...)"
  (let ((var (car spec))
        (count (cadr spec))
        (limit-sym (gensym)))
    `(let ((,limit-sym ,count))
       (do ((,var 0 (+ ,var 1)))
           ((>= ,var ,limit-sym) nil)
         ,@body))))

(defmacro dolist (spec . body)
  "Iteriert über Liste: (dolist (x lst) ...)"
  (let ((var (car spec))
        (lst (cadr spec))
        (lst-sym (gensym)))
    `(let ((,lst-sym ,lst))
       (do ((,var (car ,lst-sym) (car rest))
            (rest (cdr ,lst-sym) (cdr rest)))
           ((null? rest) nil)
         ,@body))))

;; ---------------------------------------------------------------------------
;; Struktur-Definition (wie defstruct in Common Lisp)
;; ---------------------------------------------------------------------------

(defmacro defstruct (name-and-options . fields)
  "Definiert eine Struktur mit Konstruktor, Prädikat und Accessoren"
  (let* ((name (if (atom name-and-options) name-and-options (car name-and-options)))
         (name-str (symbol->string name))
         (constructor (string->symbol (string-append "make-" name-str)))
         (predicate (string->symbol (string-append name-str "?")))
         (conc-name (string-append name-str "-")))

    `(begin
       ;; Konstruktor
       (defun ,constructor (,@fields)
         (list ',name ,@fields))

       ;; Prädikat
       (defun ,predicate (obj)
         (and (list? obj) (eq (car obj) ',name)))

       ;; Accessoren für jedes Feld
       ,@(let loop ((fs fields) (idx 1))
           (if (null? fs)
               '()
               (cons
                `(defun ,(string->symbol
                          (string-append conc-name (symbol->string (car fs)))) (obj)
                   (nth ,idx obj))
                (loop (cdr fs) (+ idx 1)))))

       ;; Rückgabe des Strukturnamens
       ',name)))

;; Hilfsfunktion für list? Prädikat (falls nicht vorhanden)
(defun list? (x)
  "Prüft ob x eine Liste ist"
  (or (null? x) (and (cons? x) t)))

;; ---------------------------------------------------------------------------
;; Debugging-Makros
;; ---------------------------------------------------------------------------

(defmacro assert (condition . opt-msg)
  "Prüft Bedingung, gibt Fehler wenn falsch"
  (let ((msg (if (null? opt-msg)
                 (string-append "Assertion failed: " (symbol->string condition))
                 (car opt-msg))))
    `(if ,condition
         t
         (error ,msg))))

;; ---------------------------------------------------------------------------
;; Funktionale Programmierung
;; ---------------------------------------------------------------------------

(defmacro compose (f g)
  "Erzeuget Komposition (f ∘ g)"
  `(lambda (x) (,f (,g x))))

(defmacro curry (fn . args)
  "Teilweise Anwendung – fixiert linke Argumente"
  `(lambda rest-args
     (apply ,fn (append (list ,@args) rest-args))))

;; ---------------------------------------------------------------------------
;; Meta-Makros (Makros die Makros generieren)
;; ---------------------------------------------------------------------------

(defmacro defsynonym (new-name old-name)
  "Definiert Synonym für bestehende Funktion/Makro"
  `(defmacro ,new-name args
     `(,',old-name ,@args)))

(defmacro define-with-doc (name docstring value)
  "Definiert Variable mit Dokumentation (als Metadaten-Comment)"
  `(define ,name
     ;; ,docstring
     ,value))

;; ---------------------------------------------------------------------------
;; Code-Generierungs-Hilfen
;; ---------------------------------------------------------------------------

(defmacro with-gensyms (names . body)
  "Bindet frische Symbole für hygienische Makros"
  `(let ,(mapcar (lambda (n) (list n '(gensym))) names)
     ,@body))

(defmacro with-unique-names (names . body)
  "Alias für with-gensyms"
  `(with-gensyms ,names ,@body))

;; ---------------------------------------------------------------------------
;; Beispiel: Verwendung der Makros
;; ---------------------------------------------------------------------------

(println "macro-utils.lisp geladen – Makro-Bibliothek verfügbar")
(println "Verfügbare Makros:")
(println "  when, unless, cond – Kontrollfluss")
(println "  let*, dotimes, dolist – Variablenbindung/Schleifen")
(println "  defstruct – Strukturdefinition")
(println "  assert – Debugging")
(println "  compose, curry – Funktionale Programmierung")
(println "  with-gensyms – Hygienische Makros")
