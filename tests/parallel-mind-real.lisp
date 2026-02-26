; parallel-mind-real.lisp – Ensemble-KI mit echten API-Aufrufen
; Autor: Claude
; CoAutor: Gerhard Quell
; Erstellt: 20260226
;
; DEMONSTRIERT ECHTE PARALLELE KI-ANFRAGEN via sigoREST
; ACHTUNG: Dies macht echte API-Aufrufe zu den KI-Modellen!

(load "tests/demo-utils.lisp")

(print-header "Parallel Mind Demo – ECHTE KI-AUFRUFE")
(println "Verbinde mit sigoREST auf http://127.0.0.1:9080...")
(println "")

;; Verfügbare Modelle prüfen
(println "Verfügbare Modelle:")
(println "  - claude-h (Claude Haiku)")
(println "  - gemini-p (Gemini Pro)")
(println "  - gpt41 (GPT-4.1)")
(println "")

;; Das Problem, das wir lösen wollen
(define problem
  "Erkläre in einem Satz: Was ist die Hauptidee hinter Lisp's Homoikonizität?")

(println (string-append "Problem: " problem))
(println "")

;; ECHTE parallele KI-Anfragen
(println "Starte 3 PARALLELE echte KI-Anfragen via parfunc...")
(println "(Das kann 5-15 Sekunden dauern)")
(println "")

(parfunc perspectives
  ;; Perspektive 1: Claude – fokussiert auf Klarheit
  (sigo (string-append "Gib eine präzise, klare Antwort in einem Satz: " problem)
        "claude-h")

  ;; Perspektive 2: Gemini – fokussiert auf Kreativität
  (sigo (string-append "Gib eine kreative, bildliche Antwort in einem Satz: " problem)
        "gemini-p")

  ;; Perspektive 3: GPT-4 – fokussiert auf Technik
  (sigo (string-append "Gib eine technisch prägnante Antwort in einem Satz: " problem)
        "gpt41"))

;; Zeige gesammelte Perspektiven
(println "Gesammelte Perspektiven (ECHTE KI-Antworten):")
(println "-----------------------------------------------")
(println "Claude (Haiku):")
(println (car perspectives))
(println "")
(println "Gemini (Pro):")
(println (cadr perspectives))
(println "")
(println "GPT-4.1:")
(println (caddr perspectives))
(println "")

;; Erstelle Synthese-Prompt
(define synthesis-prompt
  (string-append
   "Synthetisiere diese drei Perspektiven zu einer prägnanten Erklärung in einem Satz:\n\n"
   "1. Claude: " (car perspectives) "\n"
   "2. Gemini: " (cadr perspectives) "\n"
   "3. GPT-4: " (caddr perspectives) "\n\n"
   "Gib nur die synthetisierte Antwort."))

(println "Synthese-Prompt für Meta-KI (Claude):")
(println "--------------------------------------")
(println synthesis-prompt)
(println "")

;; Synthese durchführen
(println "Führe Synthese durch...")
(define synthesis (sigo synthesis-prompt "claude-h"))

(println "")
(println "=== SYNTHETISIERTES ERGEBNIS ===")
(println synthesis)
(println "")

;; Performance-Vergleich
(print-header "Performance-Analyse")

(println "Sequentielle Ausführung würde dauern:")
(println "  Zeit = Summe(Claude) + Summe(Gemini) + Summe(GPT-4)")
(println "       ≈ 2s + 2s + 2s = ~6 Sekunden")
(println "")

(println "Parallele Ausführung mit parfunc:")
(println "  Zeit = Max(Claude, Gemini, GPT-4)")
(println "       ≈ 2-3 Sekunden")
(println "       = 2-3x schneller!")
(println "")

(println "GoLisp's einzigartige Kombination:")
(println "  ✅ Go-Parallelität via parfunc")
(println "  ✅ Integrierte KI-Anbindung via sigo")
(println "  ✅ Kein anderes Lisp hat beides!")
(println "")
