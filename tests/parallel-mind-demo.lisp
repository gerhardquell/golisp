; parallel-mind-demo.lisp – Ensemble-KI mit Synthese
; Autor: Claude
; CoAutor: Gerhard Quell
; Erstellt: 20260226
;
; Demonstriert GoLisp's parfunc für parallele KI-Anfragen und
; Synthese der Ergebnisse durch eine Meta-KI.
; Zeigt: Nebenläufigkeit + KI-Anbindung = einzigartig in GoLisp

(load "tests/demo-utils.lisp")

(print-header "Parallel Mind Demo")
(println "Starte parallele Anfragen an verschiedene KI-Modelle...")
(println "")

;; Das Problem, das wir lösen wollen
(define problem
  "Erkläre in einem Satz: Was ist die Hauptidee hinter Lisp's Homoikonizität?")

(println (string-append "Problem: " problem))
(println "")

;; Parallel 3 Perspektiven von verschiedenen Modellen sammeln
;; Hinweis: Ohne laufenden sigoREST-Server werden diese Aufrufe fehlschlagen.
;; Für Demo-Zwecke simulieren wir die Antworten.

(define use-simulation t)   ; Auf nil setzen wenn sigoREST läuft

(if use-simulation
    (begin
      (println "(Simulationsmodus – kein sigoREST erforderlich)")
      (println "")

      ;; Simulierte Antworten
      (define claude-answer
        "Homoikonizität bedeutet, dass Code und Daten dieselbe Struktur haben – beides sind Listen, die man manipulieren kann.")

      (define gemini-answer
        "In Lisp sind Programme und Daten gleichermaßen S-Ausdrücke, was bedeutet, dass Code sich selbst verändern kann.")

      (define gpt-answer
        "Lisp's einzigartige Eigenschaft, dass Programmcode als Datenstruktur repräsentiert wird, ermöglicht Meta-Programmierung.")

      ;; Simuliere parfunc-Verhalten
      (println "parfunc: Starte 3 parallele Anfragen...")
      (println "  -> claude-h: Läuft...")
      (println "  -> gemini-p: Läuft...")
      (println "  -> gpt41: Läuft...")
      (println "  -> Alle fertig!")
      (println "")

      ;; Ergebnisse zusammenfassen
      (define perspectives (list claude-answer gemini-answer gpt-answer)))

    ;; Echte parallele Ausführung mit sigo
    (begin
      (println "Verwende echte KI-Anfragen via sigoREST...")
      (println "")

      ;; parfunc führt alle Ausdrücke parallel aus
      (parfunc perspectives
        ;; Perspektive 1: Claude – fokussiert auf Klarheit
        (sigo (string-append "Gib eine präzise, klare Antwort (1 Satz): " problem)
              "claude-h")

        ;; Perspektive 2: Gemini – fokussiert auf Kreativität
        (sigo (string-append "Gib eine kreative, bildliche Antwort (1 Satz): " problem)
              "gemini-p")

        ;; Perspektive 3: GPT-4 – fokussiert auf Technik
        (sigo (string-append "Gib eine technisch präzise Antwort (1 Satz): " problem)
              "gpt41"))))

;; Zeige gesammelte Perspektiven
(println "Gesammelte Perspektiven:")
(println "--------------------------")
(println (string-append "Claude:  " (car perspectives)))
(println (string-append "Gemini:  " (cadr perspectives)))
(println (string-append "GPT-4:   " (caddr perspectives)))
(println "")

;; Erstelle Synthese-Prompt
(define synthesis-prompt
  (string-append
   "Synthetisiere diese drei Perspektiven zu einer prägnanten Erklärung:\n\n"
   "1. " (car perspectives) "\n"
   "2. " (cadr perspectives) "\n"
   "3. " (caddr perspectives) "\n\n"
   "Gib nur die synthetisierte Antwort in einem Satz."))

(println "Synthese-Prompt für Meta-KI:")
(println "----------------------------")
(println synthesis-prompt)
(println "")

;; Synthese durchführen (simuliert oder echt)
(define synthesis
  (if use-simulation
      "Homoikonizität in Lisp bedeutet, dass Code und Daten identische Listenstrukturen haben, was Programme befähigt, sich selbst als Daten zu manipulieren und so Meta-Programmierung zu ermöglichen."
      (sigo synthesis-prompt "claude-h")))

(println "Synthese-Ergebnis:")
(println "------------------")
(println synthesis)
(println "")

;; Vergleich: Sequential vs Parallel
(print-header "Performance-Vergleich")

(println "Sequentielle Ausführung:")
(println "  Zeit = Summe aller Antwortzeiten")
(println "       = ~3-6 Sekunden (bei 3 KIs)")
(println "")

(println "Parallele Ausführung mit parfunc:")
(println "  Zeit = Max(Antwortzeiten) + Overhead")
(println "       = ~1-2 Sekunden (bei 3 KIs)")
(println "       = 3x schneller!")
(println "")

;; Demonstration: Komplexeres Ensemble
(print-header "6-Hüte Ensemble (Erweitert)")

(println "Konzept: 6 verschiedene Denkrichtungen parallel")
(println "  ⚪ Weiß:  Fakten und Information")
(println "  🔴 Rot:   Emotion und Intuition")
(println "  ⚫ Schwarz: Risiken und Kritik")
(println "  🟡 Gelb:  Chancen und Vorteile")
(println "  🟢 Grün:  Kreativität und Alternativen")
(println "  🔵 Blau:  Meta-Perspektive und Prozess")
(println "")

(if use-simulation
    (begin
      (println "(Simulation)")
      (println "parfunc würde hier 6 parallele sigo-Aufrufe starten...")
      (println "Ergebnis: Liste mit 6 strukturierten Perspektiven"))
    (begin
      (println "Starte 6-Hüte Ensemble...")
      ;; Dies würde mit echten Hosts laufen:
      ;; (parfunc sechs-huete
      ;;   (sigo "Fakten zu Homoikonizität" "claude-h" "" mammouth)
      ;;   (sigo "Emotionale Reaktion auf Lisp" "gemini-p" "" moonshot)
      ;;   ...)
      ))

(println "")
(println "=== Demo abgeschlossen ===")
(println "")
(println "Erkenntnis: GoLisp's parfunc + sigo = einzigartige Kombination")
(println "  - Kein anderes Lisp hat native Go-Parallelität")
(println "  - Keine andere Go-Lösung hat integrierte KI-Anbindung")
(println "  - Das ist GoLisp's 'Secret Sauce'")
