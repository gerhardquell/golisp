; run-all-demos.lisp – Test-Runner für alle GoLisp Demos
; Autor: Claude
; CoAutor: Gerhard Quell
; Erstellt: 20260226
;
; Lädt und führt alle Demos sequentiell aus.
; Verwendung: ./golisp tests/run-all-demos.lisp

(println "")
(println "╔══════════════════════════════════════════════════════════════╗")
(println "║         GoLisp Demo-Suite – Stärken-Showcase                ║")
(println "║     Go-Parallelität + Lisp-Homoikonizität + KI             ║")
(println "╚══════════════════════════════════════════════════════════════╝")
(println "")

;; ============================================================================
;; Demo 1: Parallel Mind
;; ============================================================================

(println "")
(println "▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶")
(println "  DEMO 1: Parallel Mind – Ensemble-KI mit Synthese")
(println "  Zeigt: parfunc + sigo = parallele KI-Anfragen")
(println "▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶")
(println "")

(load "tests/parallel-mind-demo.lisp")

;; ============================================================================
;; Demo 2: Macro Evolution
;; ============================================================================

(println "")
(println "▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶")
(println "  DEMO 2: Macro Evolution – Selbstmodifizierender Code")
(println "  Zeigt: defmacro + quasiquote + eval = Code generiert Code")
(println "▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶")
(println "")

(load "tests/macro-evolution-demo.lisp")

;; ============================================================================
;; Demo 3: Genetic Programming
;; ============================================================================

(println "")
(println "▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶")
(println "  DEMO 3: Genetic Lisp – Evolutionärer Code-Generator")
(println "  Zeigt: Code = Daten + Evolution = automatische Programmierung")
(println "▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶▶")
(println "")

(load "tests/genetic-programming-demo.lisp")

;; ============================================================================
;; Zusammenfassung
;; ============================================================================

(println "")
(println "╔══════════════════════════════════════════════════════════════╗")
(println "║                     Alle Demos abgeschlossen!                ║")
(println "╠══════════════════════════════════════════════════════════════╣")
(println "║                                                              ║")
(println "║  GoLisp's einzigartige Kombination:                          ║")
(println "║                                                              ║")
(println "║  1. 🚀 Go-Parallelität    – parfunc für echte Nebenläufigkeit║")
(println "║  2. 📝 Lisp-Homoikonizität – Code als manipulierbare Daten   ║")
(println "║  3. 🤖 KI-Anbindung       – sigo für intelligente Erweiterung║")
(println "║                                                              ║")
(println "║  Das Ergebnis: Ein selbsterweiterndes System, das sich       ║")
(println "║  durch KI-Calls selbst vervollständigen kann.                ║")
(println "║                                                              ║")
(println "╚══════════════════════════════════════════════════════════════╝")
(println "")
(println "Einzelne Demos können mit folgenden Befehlen gestartet werden:")
(println "  ./golisp tests/parallel-mind-demo.lisp")
(println "  ./golisp tests/macro-evolution-demo.lisp")
(println "  ./golisp tests/genetic-programming-demo.lisp")
(println "")
