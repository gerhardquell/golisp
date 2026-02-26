; gp-targets.lisp – Zielfunktionen für Genetic Programming
; Autor: Claude
; CoAutor: Gerhard Quell
; Erstellt: 20260226
;
; Diese Datei enthält Zielfunktionen, die der GP-Algorithmus
; zu approximieren versucht.

;; ============================================================================
;; Einfache mathematische Funktionen
;; ============================================================================

;; Quadratische Funktion: f(x) = x² + 2x + 1 = (x+1)²
(defun target-quadratic (x)
  "Zielfunktion: Quadratische Funktion"
  (+ (* x x) (* 2 x) 1))

;; Kubische Funktion: f(x) = x³ - 2x² + x
(defun target-cubic (x)
  "Zielfunktion: Kubische Funktion"
  (- (+ (* x x x) x) (* 2 (* x x))))

;; Lineare Funktion: f(x) = 3x + 5
(defun target-linear (x)
  "Zielfunktion: Lineare Funktion"
  (+ (* 3 x) 5))

;; ============================================================================
;; Trigonometrische Approximationen (Taylor-Reihen)
;; ============================================================================

;; Approximation von sin(x) für kleine x: x - x³/6
(defun target-sin-approx (x)
  "Zielfunktion: Sinus-Approximation"
  (- x (/ (* x x x) 6)))

;; Approximation von e^x: 1 + x + x²/2
(defun target-exp-approx (x)
  "Zielfunktion: Exponential-Approximation"
  (+ 1 x (/ (* x x) 2)))

;; ============================================================================
;; Mehrere Variablen
;; ============================================================================

;; f(x,y) = x² + y²
(defun target-circle (x y)
  "Zielfunktion: Kreis-Gleichung (x² + y²)"
  (+ (* x x) (* y y)))

;; f(x,y) = x*y + x + y
(defun target-interaction (x y)
  "Zielfunktion: Interaktionsterm"
  (+ (* x y) x y))

;; ============================================================================
;; Boolean-Funktionen (für symbolische Regression)
;; ============================================================================

;; XOR: f(a,b) = (a OR b) AND NOT (a AND b)
;; Wir repräsentieren booleans als 0/1
(defun target-xor (a b)
  "Zielfunktion: XOR (0 oder 1)"
  (if (and (or (= a 1) (= b 1))
           (not (and (= a 1) (= b 1))))
      1
      0))

;; ============================================================================
;; Test-Daten-Generatoren
;; ============================================================================

(defun generate-test-points (count min-val max-val)
  "Generiert Count Testpunkte zwischen min und max"
  (let ((step (/ (- max-val min-val) count)))
    (mapcar (lambda (i)
              (+ min-val (* i step)))
            (range 0 count))))

;; Standard-Testpunkte
(define *test-points-1d* (generate-test-points 10 -5 5))
(define *test-points-small* (generate-test-points 5 -2 2))

;; ============================================================================
;; Fitness-Hilfsfunktionen
;; ============================================================================

(defun calculate-error (individual target-fn test-points)
  "Berechnet den durchschnittlichen Fehler eines Individuums"
  (/ (sum (mapcar (lambda (x)
                    (let ((expected (target-fn x))
                          (actual (eval-with-x individual x)))
                      (abs (- expected actual))))
                  test-points))
     (length test-points)))

(defun eval-with-x (expr x-val)
  "Evaluiert expr mit x = x-val"
  (eval `(let ((x ,x-val)) ,expr)))

;; ============================================================================
;; Zielfunktions-Metadaten
;; ============================================================================

(define *target-functions*
  '((quadratic . target-quadratic)
    (cubic . target-cubic)
    (linear . target-linear)
    (sin-approx . target-sin-approx)
    (exp-approx . target-exp-approx)))

(defun get-target-by-name (name)
  "Gibt Zielfunktion nach Namen zurück"
  (cdr (assoc name *target-functions*)))

(println "gp-targets.lisp geladen")
(println "Verfügbare Zielfunktionen:")
(println "  - target-quadratic (x² + 2x + 1)")
(println "  - target-cubic (x³ - 2x² + x)")
(println "  - target-linear (3x + 5)")
(println "  - target-sin-approx (x - x³/6)")
(println "  - target-exp-approx (1 + x + x²/2)")
(println "  - target-circle (x² + y²)")
(println "  - target-interaction (xy + x + y)")
(println "  - target-xor (XOR für 0/1)")
