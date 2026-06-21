# GoLisp – Retrospektive

**Datum:** 2026-02-24
**Autoren:** Gerhard Quell & Claude Sonnet 4.6

---

## Was haben wir gebaut?

| Feature | Dateien | Commits |
|---------|---------|---------|
| Quasiquote `` ` `` `,` `,@` | `reader.go`, `eval.go` | 1 |
| `apply` | `primitives.go` | 1 |
| `cond` | `eval.go` | 1 |
| 7 String-Funktionen | `stringfuncs.go` (neu) | 1 |
| TCO (Tail-Call-Optimierung) | `eval.go` | 4 |
| `gensym` | `primitives.go` | 2 |
| `(error msg)` + `(catch body handler)` | `types.go`, `eval.go`, `primitives.go` | 5 |

**Gesamt:** 7 Features, ~15 Commits, ~600 neue Zeilen Go-Code.

**Vorher:** Stack-Overflow bei tiefer Rekursion, keine Makro-Hygiene, kein Error Handling.
**Nachher:** 1.000.000 Rekursionen in 44ms, hygienische Makros mit `gensym`, strukturiertes Error Handling.

---

## Was lief gut?

### Plan → Execute → Review Workflow
Das dreigliedrige Muster (Plan schreiben → Subagent implementiert → zwei Review-Stufen)
hat sich bewährt. Echte Bugs wurden konsequent abgefangen, bevor sie in `main` landeten:

- TCO: `else`-Branch verwendete `Cdr.Cdr.Car` statt `Cdr.Cdr.Cdr.Car` → abgefangen
- TCO: fehlende Typprüfung vor `fn.Fn(args)` → nil-Panic verhindert
- gensym: fehlender Arity-Check → ergänzt
- Error Handling: `LispError.Error()` leer für Nicht-String-Zellen → behoben
- Error Handling: `%v` statt `%w` in `evalLoad` → `errors.As` funktioniert jetzt

### TDD-Rhythmus
Failing test zuerst schreiben hat bei TCO den Wert klar bewiesen:
der Test crashte mit Stack-Overflow — unbestreitbarer Beweis dass TCO nötig ist.

### Subagent-Reviews als Qualitätsstufe
Die Kombination aus Spec-Review und Code-Quality-Review hat verhindert,
dass "es läuft grob" als "fertig" durchgeht.

---

## Was lief nicht so gut?

### Language-Server false positives
Mehrfach wurden Compiler-Diagnostiken gemeldet (`sync/atomic` "unused",
`errors` "unused", `evalCatch undefined`), die bei `go build` nicht auftraten.
Ursache: der Language-Server analysiert Dateien einzeln und erkennt
cross-file Abhängigkeiten nicht sofort nach einer Änderung.

**Lösung:** Immer zuerst `go build ./...` als Ground Truth.

### Spec-Reviewer verstand TDD-Phasen nicht
Bei Task 1 von Error Handling meldete der Spec-Reviewer `catch` als fehlend —
obwohl der Plan explizit "Tests sollen hier noch fehlschlagen" vorsah.

**Lösung:** In Implementer-Prompts explizit vermerken welche Tests in welcher Phase
grün/rot sein sollen.

### Plan-Lücke bei `evalLoad`
Der `%v` vs `%w` Fehler in `evalLoad` wäre vermeidbar gewesen,
wenn der Plan `errors.As` im Kontext der gesamten Fehlerkette betrachtet hätte.

**Lösung:** Bei `errors.As` / `errors.Is` immer prüfen ob Fehler durch
Wrapping-Schichten propagieren.

---

## Technische Erkenntnisse

### TCO in Go
Go's goroutine-Stacks wachsen automatisch bis 1 GB — aber 4 Go-Frames
pro Lisp-Rekursionsschritt × 1.000.000 Aufrufe = ~400 MB Stack-Bedarf.
TCO via `for`-Loop reduziert das auf O(1).

Das Loop-Muster ist sauberer als Trampolin: kein `continue`-Label nötig,
kein Thunk-Overhead, kein separater Dispatcher.

### LispError als eigener Go-Typ
`*LispError` als Go-Typ (statt nur `fmt.Errorf`) erlaubt präzises
Type-Switching in `catch` — nur Lisp-Fehler werden abgefangen,
interne Go-Fehler (z.B. Division durch 0 im Go-Layer) propagieren unverändert.
Das gibt dem System klare Semantik.

### Quasiquote-Tiefe
`evalQQ(expr, env, depth)` mit depth-Parameter löst verschachtelte
Quasiquotes korrekt: `\`(a \`(b ,(+ 1 2)))` expandiert nur die innere
Ebene, nicht die äußere.

---

## Noch offen nach Session 1

*(alle in Session 2 abgearbeitet)*

---

## Fazit Session 1

GoLisp hat sich in einer Session von einem funktionalen Prototypen
zu einem ernsthaften Lisp-Interpreter entwickelt.
Der Workflow (Plan → Subagent → Review) hat gezeigt, dass
KI-getriebene Entwicklung mit klaren Qualitätsgates
konsistent gute Ergebnisse liefert — nicht trotz der Reviews,
sondern wegen ihnen.

> "Code = Daten + KI = sich selbst erweiterndes System"
> — Gerhard & Claude, Februar 2026

---
---

# Session 2 – 2026-02-24

**Autoren:** Gerhard Quell & Claude Sonnet 4.6

---

## Was haben wir gebaut?

| Feature | Dateien | Commits |
|---------|---------|---------|
| Multi-Body `defun`/`lambda`/`defmacro` via `wrapBegin` | `eval.go` | 2 |
| `>=` `<=` Vergleichsoperatoren | `primitives.go` | 1 |
| History-Persistenz `~/.golisp_history` | `readline.go`, `env.go` | 1 |
| REPL-Rewrite: `go-prompt`, Syntax-Highlighting | `readline.go` | 2 |
| `while` Schleife | `eval.go` | 1 |
| `do` Schleife (Scheme-style) | `eval.go` | 1 |
| TCO-Regressionstests (war bereits implementiert) | `main.go` | 1 |
| `equal?` struktureller Vergleich | `primitives.go` | 1 |
| `CLAUDE.md` + `BESCHREIBUNG.md` | Docs | 2 |

**Gesamt:** 8 Features + 2 Docs, 12 Commits, ~350 neue Zeilen.

**Vorher:** Single-Body defun, kein Highlighting, keine `>=`/`<=`, keine `do`/`while`, kein `equal?`.
**Nachher:** Vollständige Sprache, farbiger REPL, alle Standard-Lisp-Features implementiert.

---

## Was lief gut?

### TDD als Entdeckungswerkzeug
Bei TCO: die Tests liefen sofort grün — was beweist, dass TCO bereits in der
Vorjahressession implementiert war. TDD hat hier nicht eine neue Implementierung
erzwungen, sondern eine fehlerhafte Speicherlücke (MEMORY.md) korrigiert.
Das ist der eigentliche Wert: Tests als objektive Wahrheitsquelle.

### go-prompt API-Recherche vor dem Coden
Statt blind drauflos zu programmieren, wurden zuerst die Quelldateien der Library
gelesen (`lexer.go`, `constructor.go`, Beispiele). Das ersparte mehrere Iterationen:
die `EagerLexer` / `LexerFunc`-Signatur, `ExecuteOnEnterCallback` für Multi-line
und `WithCustomHistory` für Persistenz — alles auf Anhieb korrekt.

### Minimale Änderungen
`wrapBegin` — 10 Zeilen, 3 Aufrufstellen geändert, kein neuer Eval-Pfad.
`cellEqual` — 12 Zeilen, rekursiv, deckt alle Typen ab.
Beide Features hätten auch mit doppelt so viel Code implementiert werden können —
die Minimalform ist robuster und leichter zu verstehen.

---

## Was lief nicht so gut?

### go-prompt Completion-Popup
Das Auswahlfeld erschien automatisch beim Tippen — unerwartet und störend.
`go-prompt` kennt kein "nur auf TAB anzeigen"-Flag, die Lösung war,
den Completer komplett zu entfernen.

**Erkenntnis:** Bei Library-Auswahl für UI-Features vorab prüfen
ob das gewünschte Verhalten (TAB-only) überhaupt konfigurierbar ist.

### Farben nicht sichtbar
Die ersten Bracket-Farben (Yellow/Cyan/Green) waren auf Gerhards Terminal
nicht erkennbar. Zwei Iterationen nötig bis Red/Green/Yellow/Fuchsia passte.

**Erkenntnis:** Terminal-Farbpaletten variieren stark. Bei Farb-Features
früh fragen welches Terminal / welcher Hintergrund verwendet wird.

### fileHistory Workaround
Der erste `newFileHistory`-Entwurf enthielt einen dummy-Aufruf
`prompt.WithHistory(entries)((*prompt.Prompt)(nil))` — sah nach einem Hack aus
und wurde sofort bereinigt. Ursache: die Library-API für "History vorladen"
war nicht sofort offensichtlich und die direkte `Add`-Loop war die sauberere Lösung.

---

## Technische Erkenntnisse

### wrapBegin als Normalisierungsschritt
Multi-Body zur *Definitionszeit* in `(begin ...)` wrappen ist eleganter als
zur Laufzeit: der Evaluator bleibt unverändert, `begin` ist bereits TCO-aware,
und der Overhead für Single-Body-Funktionen ist null (kein Wrapper).

### go-prompt ExecuteOnEnterCallback
`p.Buffer().Text()` im Callback liefert den gesamten bisherigen Multi-line-Buffer.
`countDepth` darauf angewandt ergibt direkt ob der Ausdruck vollständig ist.
Rückgabe `(depth, false)` → go-prompt rückt automatisch ein, kein manuelles
`..`-Prompt mehr nötig.

### do mit gleichzeitigem Step-Update
Scheme's `do` evaluiert alle Step-Ausdrücke im *alten* Environment bevor
die neuen Werte gesetzt werden:
```lisp
(do ((a 1 b) (b 2 a)) ((= a 3) (list a b)))  ; → (2 1), nicht (2 2)
```
Die Implementierung sammelt daher zuerst alle neuen Werte in einem Slice,
setzt sie dann gesammelt. Das ist der semantisch korrekte Scheme-Weg.

---

## Zustand der Sprache

Nach Session 2 ist GoLisp **feature-complete** für einen ernsthaften Lisp-Interpreter:

- ✅ Alle Standard-Spezialformen
- ✅ Quasiquote / Makros / gensym
- ✅ Error Handling (error/catch)
- ✅ TCO — beliebig tiefe Tail-Rekursion
- ✅ Multi-Body defun/lambda/defmacro
- ✅ Schleifen (while, do)
- ✅ Strukturelle Gleichheit (equal?)
- ✅ Vollständige String-Bibliothek (UTF-8)
- ✅ Datei-I/O
- ✅ Nebenläufigkeit (parfunc, channels, locks)
- ✅ KI-Anbindung (sigo/sigoREST)
- ✅ REPL mit Syntax-Highlighting und History

**Nächste Ausbaustufen** (offen, kein Zeitdruck):
- `string-ref`, `string-split` — weitere String-Operationen
- `number?`, `string?`, `list?` — Typprädikate
- Varargs in defun: `(defun f (x . rest) ...)`
- Mehrwertrückgabe (values/call-with-values)

---

## Fazit Session 2

Die Sprache ist vollständig. Der REPL macht Spaß.
Das Fundament ist stabil genug für das eigentliche Ziel:
GoLisp als selbsterweiterndes KI-System.

> "Eine Sprache die sich selbst erweitern kann,
>  braucht zuerst eine Sprache die vollständig ist."
> — Gerhard & Claude, Februar 2026

---
---

# Session 3 – 2026-02-24

**Autoren:** Gerhard Quell & Claude Sonnet 4.6

---

## Was haben wir gebaut?

| Feature | Dateien | Commits |
|---------|---------|---------|
| `fnEq` redundanten Vergleich fixen | `primitives.go` | 1 |
| `macroexpand` als Debugging-Hilfe | `eval.go` | 1 |

**Gesamt:** 2 Quick Wins, 1 Commit, ~35 Zeilen geändert.

**Vorher:** `fnEq` verglich unnötigerweise auch `Val`; keine Möglichkeit Makro-Expansion zu inspizieren.
**Nachher:** `fnEq` vergleicht nur noch `Num`; `macroexpand` zeigt expandierte Makros.

---

## Was lief gut?

### Plan → Execute ohne Review-Overhead
Die drei Quick Wins waren klein und sicher genug für direkte Implementierung:
- `fnEq`: offensichtlicher Bug, einfache Lösung
- `macroexpand`: klare Spezifikation, saubere Architektur-Entscheidung

Keine Subagenten nötig – der Aufwand wäre größer als der Nutzen.

### Spezialform statt Primitive
Die erste `macroexpand`-Implementierung als Primitive (`primitives.go`) scheiterte elegant:
Primitives haben keinen `env`-Zugriff, können also Makros nicht auflösen.
Die Umstellung auf Spezialform (`eval.go`) war der korrekte Architektur-Pfad.

**Bestätigte Regel:** *Braucht Zugriff auf `env`? → Spezialform. Reine Berechnung? → Primitive.*

---

## Was lief nicht so gut?

### Keine nennenswerten Probleme
Alle drei Quick Wins funktionierten auf Anhieb:
- Build erfolgreich
- Alle 40 Tests grün
- `macroexpand` expandiert `when` korrekt zu `if`

---

## Technische Erkenntnisse

### `macroexpand` als Debugging-Werkzeug
Die Fähigkeit Makros zu expandieren ist essentiell für Makro-Entwicklung:

```lisp
> (macroexpand '(when x y))
(if x (begin y))
```

Dies zeigt ob ein Makro korrekt expandiert ohne es auszuführen.

### Go-Idiom: Nicht mehr prüfen als nötig
`fnEq` verglich vorher `Num && Val`. Da `=` nur für Zahlen gedacht ist,
reicht der `Num`-Vergleich. Weniger Code, klarere Semantik.

---

## Fazit Session 3

Kleine, gezielte Verbesserungen mit sofort sichtbarem Nutzen.
Die Codebasis bleibt sauber, die Sprache wird benutzerfreundlicher.

> "Quick Wins sind das Öl einer Codebasis –
>  kleine Investition, große Wirkung auf Geschwindigkeit und Moral."
> — Gerhard & Claude, Februar 2026

---
---

# Session 4 – Unix-Style CLI

Siehe [`docs/retrospectives/2026-02-25-unix-cli.md`](docs/retrospectives/2026-02-25-unix-cli.md)

---
---

# Session 5 – GoLisp Server (golispd)

**Datum:** 1. März 2026
**Autoren:** Gerhard Quell & Claude Sonnet 4.6

---

## Was haben wir gebaut?

Ein vollständiger Client-Server-Stack für professionelle Lisp-Entwicklung:

| Feature | Dateien | Commits |
|---------|---------|---------|
| TCP-Server (`golispd`) | `cmd/golispd/main.go`, `lib/swank/server.go` | 1 |
| Protokoll-Handler | `lib/swank/protocol.go` | 1 |
| CLI-Client (`golisp-client`) | `cmd/golisp-client/main.go` | 1 |
| Hilfsfunktionen | `lib/types_helpers.go` | 1 |
| Dokumentation | `CLAUDE.md`, `docs/retrospective-golispd-20250301.md` | 2 |

**Gesamt:** 5 neue Dateien, ~900 Zeilen Go-Code, 6 Commits.

**Vorher:** Nur eingebetteter REPL (`./golisp -i`).
**Nachher:** Vollständiger Server mit TCP-RPC, persistenter Umgebung, IDE-fähigem Autocomplete.

---

## Was lief gut?

### Architektur-Entscheidungen

**S-Expression-RPC statt JSON**
- Natürliche Passung zu Lisp – kein zusätzlicher Parser nötig
- Menschenlesbare Protokoll-Messages für Debugging
- Der vorhandene `lib.Read()` Parser wiederverwendet

**Geteiltes Environment**
- Alle Clients sehen denselben Zustand → einfache Kollaboration
- `define` und `defun` persistieren zwischen Verbindungen
- Keine komplexe Session-Verwaltung nötig

**Klare Trennung der Verantwortlichkeiten**
- `server.go`: Netzwerk, Connection Handling
- `protocol.go`: Business Logic, Methoden-Implementierung
- `main.go` (beide): CLI, Flag-Handling

### Implementation

**Wiederverwendung bestehender Code**
- `lib.Read()`, `lib.Eval()`, `env.Symbols()` – alles vorhanden
- Nur Protokoll-Wrapper und Client-Logik neu geschrieben

**Schnelle Iteration**
- Sofortiges Testen via `echo ... | nc localhost 4321`
- Go's schnelle Compile-Zeiten ermöglichten rapid prototyping

---

## Was war herausfordernd?

### Multiline-Handling im Client

**Problem:** Newlines in Code-Strings brechen das S-Expression-Format.

**Lösung:** Escaping von `\n` zu `\\n` im Client – der vorhandene Reader handhabt das korrekt.

**Lesson Learned:** Protokoll-Design muss Whitespace berücksichtigen.

### Autocomplete für Spezialformen

**Problem:** `define`, `defun`, `if` etc. sind keine Environment-Symbole.

**Lösung:** Dokumentation klarstellt – Autocomplete zeigt nur gebundene Symbole.

---

## Technische Erkenntnisse

### S-Expression-RPC Format

```lisp
;; Request
(:id 1 :method "eval" :params ("(+ 1 2)"))

;; Response
(:id 1 :status "ok" :result "3")
;; oder
(:id 1 :status "error" :error "unbekanntes Symbol")
```

Property-Listen als natürliches Format für Lisp-Systeme.

### Goroutines pro Connection

Einfache Konkurrenz ohne manuelle Thread-Verwaltung:
```go
for server.running {
    conn, _ := listener.Accept()
    go handleConnection(conn)  // Jede Connection eigene Goroutine
}
```

---

## Fazit Session 5

GoLisp ist nun bereit für professionelle Entwicklung:
- Server-Mode für IDE-Integration (Autocomplete, Hover-Doku)
- Persistente Umgebung für langlaufende Sessions
- Client-REPL mit Multiline-Support

Der Server macht GoLisp von einem Spielzeug zu einem Werkzeug.

> "Ein Lisp ohne Server ist wie ein Klavier ohne Konzertsaal –
>  es funktioniert, aber niemand hört es."
> — Gerhard & Claude, März 2026

---
---

# Session 4 – Unix-Style CLI

Siehe [`docs/retrospectives/2026-02-25-unix-cli.md`](docs/retrospectives/2026-02-25-unix-cli.md)

---
---

# Session 6 – 2026-06-14

**Autoren:** Gerhard Quell & Claude Sonnet 4.6

---

## Was haben wir gebaut?

| Feature | Dateien | Beschreibung |
|---------|---------|--------------|
| `parfunc :timeout N` | `eval.go` | Optionaler Timeout für parallele Auswertung |
| Channel-basiertes parfunc | `eval.go` | `sync.WaitGroup` → Channel, feinere Kontrolle |
| `catch` verbessert | `eval.go` | Fängt jetzt alle Fehler ab, nicht nur `LispError` |
| `mod`, `remainder`, `abs` | `primitives.go` | Arithmetik-Primitiven |
| `random` | `primitives.go` | Zufallszahlen mit/ohne Limit |
| `string-replace`, `string-trim`, `string-contains` | `stringfuncs.go` | String-Primitiven |
| `system` | `shellcmd.go` (neu) | Shell-Kommando ausführen, Exit-Code zurück |
| `file-stat` | `shellcmd.go` (neu) | Datei-Metadaten als Assoziationsliste |
| `assoc` | `shellcmd.go` (neu) | Assoziationslisten-Suche mit `equal?` |
| `symbol->string` | `shellcmd.go` (neu) | Symbol in String konvertieren |
| sigoREST context.Timeout | `sigorest.go` | `http.Client.Timeout` → `context.WithTimeout` |
| sigoREST-Timeout 60→30s | `sigorest.go` | Realistischerer Default |

**Gesamt:** 12 Features/Fixes, 2 Commits golisp + 1 Commit sigoREST, 220+ neue Zeilen.

---

## Bug-Analyse: sigoREST `max_tokens:0`

Die interessanteste Arbeit dieser Session war eine Fehlerdiagnose über zwei Projekte.

### Symptom
```lisp
(sigo "test" "cl46-s")
=> Error: eval: sigo HTTP 400
```

### Erste (falsche) Hypothese
*Vermutung:* Mammouth liefert Anthropic-Format für neuere Claude-Modelle,
Engine parst nur OpenAI-Format → "Unexpected response format".

*Fix-Versuch:* `cfg.Type = "anthropic"` basierend auf Model-ID-Prefix.

*Ergebnis:* Fix gebrochen. `"anthropic"`-Type ändert Auth-Header von
`Authorization: Bearer` auf `x-api-key` — Mammouth lehnt das ab.

### Echte Ursache (nach direktem Mammouth-Test)
Mammouth gibt für **alle** Modelle OpenAI-Format zurück. Das Problem lag tiefer:

```
Mammouth /public/models → MaxOutputTokens = 0 für neue Modelle
                              ↓
Server: req.MaxTokens == 0 && modelInfo.MaxOutputTokens == 0
        → max_tokens: 0 im API-Request
                              ↓
Mammouth: finish_reason: "length", content: null
                              ↓
Engine: null.(string) schlägt fehl → "Unexpected response format"
```

`cl4-s` (claude-sonnet-4) funktionierte weil es noch in den `CoreModels`
mit `MaxOutputTokens: 8192` definiert war — alle neueren Modelle kamen
nur aus der Live-API ohne Token-Limits.

### Fix (2 Stellen in sigoREST)
1. `main.go`: `max_tokens` nur senden wenn `> 0`
2. `engine.go`: `content: null` gibt jetzt klare Fehlermeldung statt "Unexpected format"

---

## Was lief gut?

### Direkter API-Test als Debugging-Werkzeug
`curl` direkt gegen Mammouth (mit dem API-Key aus der Umgebung) hat die
falsche Hypothese sofort widerlegt. Ohne diesen Test hätte die falsche
Lösung länger gehalten.

### Cross-Projekt-Navigation
Das `extern/sigoREST` Symlink-Muster erlaubt, beide Projekte in einer
Session zu bearbeiten, ohne Repository-Grenzen zu verlieren.

---

## Was lief nicht so gut?

### Erste Hypothese war falsch
Die "Anthropic-Format vs OpenAI-Format"-Hypothese klang plausibel,
war aber ungeprüft. Direkter `curl`-Test hätte das früher widerlegt.

**Lesson Learned:** Bei HTTP-Fehlern immer zuerst den API-Endpoint
direkt testen, bevor Code-Änderungen gemacht werden.

### Language-Server Diagnostiken (wieder)
LSP meldet Fehler für sigoREST-Dateien weil das Modul nicht im golisp-Workspace ist.
`go build` bleibt die verlässliche Ground Truth.

---

## Technische Erkenntnisse

### `max_tokens: 0` ist ein semantischer Fehler
Die meisten LLM-APIs interpretieren `max_tokens: 0` als "0 Tokens generieren",
nicht als "Provider-Default". Das Feld weglassen ist der korrekte Weg
für "kein Limit spezifiziert".

### Channel vs WaitGroup für parallele Auswertung
`sync.WaitGroup` sammelt nur "fertig"-Signale — kein Timeout möglich.
Channel mit `select` erlaubt Timeout, Early-exit und geordnetes Mapping:

```go
type parfuncResult struct { idx int; val *Cell }
ch := make(chan parfuncResult, len(exprList))
select {
case r := <-ch: gathered[r.idx] = r.val
case <-timer:   collected = len(exprList)  // Abbruch
}
```

### sigoREST-Modelle sind runtime-dynamisch
Modelle kommen nicht aus einer statischen CSV, sondern werden beim Start
live von Provider-APIs abgerufen. Shortcodes ändern sich wenn Provider
neue Modelle deployen — Dokumentation veraltet schnell.

---

## Offene Punkte

- [ ] `sigorest.go` Default-Modell noch `ollama-gemma3-4b` — nicht mehr verfügbar
- [ ] `eval.go` hat 1003 Zeilen (CLAUDE.md-Limit: 500) — Aufteilen sinnvoll
- [ ] `postgres.go` nicht in CLAUDE.md dokumentiert

---

## Fazit Session 6

Eine Session dominiert von Debugging statt Feature-Bau. Wert lag in der
systematischen Fehleranalyse: falsche Hypothese schnell identifiziert,
echte Ursache durch direkten API-Test gefunden, Fix sauber in zwei Dateien.

> "Ein Bug der zwei Projekte überspannt, lehrt mehr als zehn Features."
> — Gerhard & Claude, Juni 2026

---

# Session 7 – 2026-06-16: Test-Netz und eval.go-Split

**Autoren:** Gerhard Quell & Claude
**Branch:** main
**Abschluss-Commit:** `7917510 Split eval.go (1003 Zeilen) in 6 kohäsive Module`

---

## Ziel

Drei offene Hoch-Prio-Punkte aus `Todo.md` aufräumen, in der Reihenfolge,
die das Risiko minimiert: erst Rätsel klären (certs), dann Sicherheitsnetz
bauen (Tests), dann am Herzstück operieren (eval.go-Split).

## Was haben wir gebaut?

| Arbeit | Ergebnis |
|--------|----------|
| `certs/`-Rätsel geklärt | Verwaistes sigoREST-Cert erkannt, gelöscht, `.gitignore`-Guard |
| Reader-Tests (Todo #3.1) | 13 Charakterisierungstests in `reader_test.go` |
| Eval-Tests (Todo #3.2) | 21 Tests in `eval_test.go`, inkl. TCO-Schutz (200k tail-rec) |
| `eval.go`-Split (Todo #1) | 1003 Zeilen → 6 Module, alle <300, reines Move |
| Atomic Commit | 11 files, +1680/−1003 |

**Tests vorher:** 2 (`env_test.go`). **Tests nachher:** 36.
**`eval.go`:** 1003 → 0 Zeilen (gelöscht, 6 neue Dateien).

---

## Was lief gut?

### Die Reihenfolge stimmte
certs → Tests → Split. Am TCO-Trampolin operieren ohne Test-Netz wäre
russisch Roulette gewesen. Das Sicherheitsnetz zuerst bauen war der
entscheidende Plan.

### Test-Netz hat sich beim ersten Split-Versuch bezahlt gemacht
Build + 36 Tests beim *ersten* Lauf nach dem Split grün. Kein einziges Mal
TCO kaputt — weil die Tests *vorher* standen, nicht weil wir Glück hatten.

### Charakterisierung statt TDD bei existierendem Code
Bei `reader.go` und `eval.go` (beide existierten schon) wurden
Charakterisierungstests geschrieben, kein TDD. Beim ersten Lauf rot:
4/13 Reader-Tests + 5/21 Eval-Tests — *alle* meine Erwartungen falsch,
keine Code-Bugs. Hätte ich TDD-Disziplin angewendet, hätte ich den Code
"repariert", um meine falsche Vermutung zu erfüllen — und echtes Verhalten
kaputtgemacht.

### Bug-Verortung in Tests
Latente Bugs (z.B. stille Typkoersion `(+ 1 "x")` = 1) wurden in Tests
sauber dem *richtigen* File zugeordnet (`primitives.go`, nicht `eval.go`).
Beim Split kein Fehlalarm — bricht ein Test, weiß ich, dass der Split schuld
ist, nicht ein zufällig mitkommender primitives-Bug.

### End-to-End-Verifikation, nicht nur Unit-Tests
lib-Tests grün allein reicht nicht. Smoke-Tests über die echte `golisp`-
Binary haben gezeigt, dass der Macro-Mechanismus wirklich läuft und TCO
in der Praxis greift (100k tail-rec → `ok`).

---

## Was nicht lief / Verbesserungspotenzial

### `-e` nimmt nur eine Expression
Erst beim Smoke-Test entdeckt: TCO- und Macro-Tests über `-e` schienen zu
"failen" (zeigten nur `defun`-Rückgabe). CLAUDE.md sagt `-e EXPR` (Singular) —
die Konsequenz (zweiter Ausdruck still ignoriert) ist nicht offensichtlich.
Für künftige manuelle Multi-Expr-Tests: stdin/Multiline nutzen.

### Erste Test-Erwartungen zu oft falsch geraten
9 von 34 Tests beim ersten Lauf rot — zwar der *Wert* von
Charakterisierungstests, zeigt aber: mein anfängliches Modell vom
GoLisp-Verhalten war lückenhaft. CLAUDE.md vorher gründlicher lesen
(NIL-Semantik, eq vs equal?, catch-Syntax) hätte einige Vermutungen
vorab korrigiert.

### `git status --cached` als Flag nicht verfügbar
Kleiner Stolperer bei der Verifikation. `git diff --cached` geht. Harmlos,
aber hätte ich wissen können.

---

## Schlüssel-Erkenntnisse

### Tail-Forms müssen inline im Eval-Loop bleiben
`if`/`begin`/`let`/`let*`/`cond`/`case` setzen `expr`/`env` und machen
`continue` — das *ist* das Trampolin. Auslagern hätte echten Funktionsaufruf
statt O(1)-Loop bedeutet → TCO kaputt → 200k-Test crasht. Nur `case`
delegiert an eine Hilfsfunktion, weil es ein Rückgabe-Tripel
`(*Cell, *Env, error)` nutzt, um env ins Trampolin zurückzureichen — der
einzige sichere Weg, eine Tail-Form auszulagern.

### Go-Tool respektiert keine `.gitignore`
Das `certs/`-Problem hätte `.gitignore` *nicht* gelöst — Go traversiert bei
`./...` jedes Unterverzeichnis, unabhängig von Git-Regeln. Nur physisches
Entfernen oder `.`/`_`-Verzeichnisprefix hilft. Häufige, gut dokumentierte Falle.

### Eine Grenze pro Kohäsions-Gruppe, nicht eine pro Zeilenzahl
`eval_specialforms.go` war nach erstem Move 313 Zeilen (über Limit). Statt
künstlich aufzuspalten, wurde `load` (thematisch I/O) nach `eval_load.go`
gezogen. Kohäsiver als mechanisches Zeilen-Splitten.

### Charakterisierungstests sind antisymmetrisch zu TDD
TDD: Test-erst (SOLL), dann Code bis grün. Charakterisierung: Code-erst
(IST), dann Tests die das IST festhalten. Falsche Raten beim Schreiben
sind der Wert — sie zeigen, wo das mentale Modell vom Code abweicht.

---

## Offene Punkte (nach dieser Session)

- [ ] **Todo #2 (hoch):** stdlib zentralisieren — `golispd` lädt inline-stdlib
  statt `//go:embed stdlib.lisp`. Drift-Gefahr zwischen zwei stdlib-Versionen.
- [ ] **Todo #3 Rest:** Primitiven-Tests, Makro-Expansion-Tests,
  parfunc/Channel-Tests.
- [ ] **Todo #5-7:** Duplikat-Bereinigung, sigoREST-Konfig, Kleinigkeiten.
- [ ] **Latenter Bug aus Eval-Tests:** Stille Typkoersion in `primitives.go`
  (`(+ 1 "x")` = 1, kein Fehler) — separater Fix, nicht eval.go.
- [x] ~~`eval.go` 1003 Zeilen~~ → aufgeteilt in 6 Module (Session 7).

---

## Fazit Session 7

Eine Session, die dem Motto "Test-Netz zuerst, dann am Herzstück operieren"
folgte — und es hat sich ausgezahlt. Der eval.go-Split lief beim ersten
Versuch grün, weil das TCO-Trampolin durch 36 Tests geschützt war, nicht
durch Glück. Drei offene Hoch-Prio-Punkte auf null reduziert (certs geklärt,
Tests gebaut, eval.go gesplittet), ein atomic Commit, sauber dokumentiert.

> "Am Trampolin operiert man nicht ohne Netz — das Netz kommt zuerst."
> — Gerhard & Claude, Juni 2026

---

# Session 8 – 2026-06-16: stdlib zentralisiert

**Autoren:** Gerhard Quell & Claude
**Branch:** main

---

## Ziel

Todo #2: `golispd` lud in `lib/swank/server.go` eine eigene inline-stdlib
(abgespeckte 20/52 Funktionen) statt der eingebetteten `stdlib.lisp` →
Drift. Server-Clients bekamen keine `iota`/`flatten`/`gcd` etc. Eine
gemeinsame Quelle schaffen.

## Was haben wir gebaut?

| Arbeit | Ergebnis |
|--------|----------|
| `stdlib.lisp` verschoben | root → `lib/stdlib.lisp` (git mv) |
| `libs/stdlib.lisp` entfernt | totes inhaltsgleiches Duplikat, untracked |
| `lib/stdlib.go` neu | `//go:embed stdlib.lisp` + zentrale `LoadStdlib(env)` |
| `main.go` umgestellt | embed+LoadString → `lib.LoadStdlib(env)` |
| `lib/swank/server.go` umgestellt | `loadStdlib()`+inline-String → `lib.LoadStdlib(s.env)` |

**Server-stdlib vorher:** 20 Funktionen (inline).
**Server-stdlib nachher:** 52 Funktionen (volle `stdlib.lisp`, wie CLI).
**server.go:** 304 → 251 Zeilen (inline-String entfernt).

## Was lief gut?

### Embed-Pfad-Limit früh erkannt
Todo-Option A ("Server auf `//go:embed` umstellen") war nicht direkt
machbar: Go verbietet `..` in embed-Pfaden, also kann `lib/swank/` nicht
auf `../../stdlib.lisp` im root zugreifen. Lösung: stdlib.lisp *selbst*
nach `lib/` verschieben, wo der Package-Baum sie erreicht. Architektur
folgt aus Tooling-Restriktion — früher erkannt, kein Sackgassen-Refactor.

### Eine Quelle, eine Funktion
Jetzt gibt es genau eine `LoadStdlib(env)` in `lib/stdlib.go` und genau
eine `stdlib.lisp`. CLI und Server rufen dieselbe Funktion auf. Drift
strukturell ausgeschlossen, nicht nur behoben.

### End-to-End über beide Binaries verifiziert
Nicht nur lib-Tests grün — sondern golispd gebaut, auf freiem Port
gestartet, und über golisp-client die ehemals fehlenden Funktionen
abgefragt: `iota`/`flatten`/`gcd`/`length`/`cadr` liefern korrekte
Ergebnisse über den Server. Drift wirklich weg, nicht nur syntaktisch.

## Was nicht lief / Verbesserungspotenzial

### gofmt-Reflex gegen CLAUDE.md
Ich habe `gofmt -w lib/stdlib.go` laufen lassen → tabs. Erst danach
zeigte `gofmt -l .`, dass **alle** lib-Files "nicht-konform" sind — GoLisp
nutzt bewusst 2-Space (CLAUDE.md: "2 Spaces, keine Tabs"). gofmt hätte
die Projektkonvention verletzt. Revertiert. **Lehre:** bei Go-Projekten
nicht reflexhaft gofmt anwenden — erst checken, ob das Projekt gofmt-
Konvention oder eigene (CLAUDE.md) hat. LSP-Linter und CLAUDE.md können
widersprüchlich sein; CLAUDE.md gewinnt.

### Env-Vorrang über Flags fiel beim Smoke-Test auf
`--port 49321` wurde durch `GOLISP_PORT=9123` aus dem Environment
übersteuert → Test-Server startete auf belegtem Port 9123 und crashte.
Erst nach explizitem `GOLISP_PORT=49321` vor dem Server-Aufruf lief es.
CLAUDE.md dokumentiert die Vorrang-Regel (env > flag) implizit. Für
Smoke-Tests: env immer explizit setzen. UX-Fund, der in die Server-Doku
gehört — kein Bug, aber eine Falle für Test-Autoren.

### `max`-Smoke-Test falsch geraten
`(max 3 7 2)` → "zu viele Argumente" — stdlib `max` nimmt nur 2 Args,
nicht variadisch. Mein Test-Input falsch, kein Code-Bug. Zeigt aber:
stdlib-Funktionen haben eigene Arity-Limits, die nirgends dokumentiert
sind. Kandidat für später (stdlib-Docstrings oder Arity-Check).

## Schlüssel-Erkenntnisse

### Drift strukturell ausschließen, nicht nur beheben
Das Problem war nicht "Server hat eine schlechte stdlib", sondern
"Server hat eine *andere* stdlib als CLI". Zwei Quellen = garantierte
Drift über die Zeit. Die Lösung ist nicht, beide inline-Strings gleich
zu halten, sondern **eine Quelle** zu schaffen. Ein `LoadStdlib`-Aufruf
an zwei Stellen kann nicht driften; zwei String-Literale an zwei Stellen
werden es.

### CLAUDE.md schlägt Linter
Projekt-Konventionen (CLAUDE.md) sind higher-priority als Standard-Tools
(gofmt). LSP/Diagnostics zeigen gofmt-Abweichungen als Warnung — aber
wenn das Projekt bewusst davon abweicht, ist die Warnung Fehlalarm.
Immer CLAUDE.md lesen *bevor* man Tool-Warnungen "repariert".

### Tooling-Restriktion bestimmt Architektur
Embed verbietet `..`-Pfade. Das ist keine Geschmacksfrage, sondern ein
hartes Go-Feature. Daraus folgt: shared Assets gehören in das Package,
das sie einbettet — nicht ins Repo-Root. stdlib.lisp in `lib/` ist nicht
nur aufgeräumt, sondern *notwendig* für `//go:embed` aus `lib/`.

---

## Offene Punkte (nach dieser Session)

- [ ] **Todo #3 Rest:** Primitiven-Tests, Makro-Expansion-Tests,
  parfunc/Channel-Tests.
- [ ] **Todo #5-7:** Duplikat-Bereinigung, sigoREST-Konfig, Kleinigkeiten.
- [ ] **Latenter Bug:** Stille Typkoersion in `primitives.go` (`(+ 1 "x")`=1).
- [ ] **Neu entdeckt:** stdlib `max`/`min` etc. nur 2-args, nicht
  variadisch — Arity-Limits undokumentiert.
- [x] ~~Todo #2 stdlib zentralisieren~~ → `LoadStdlib`, eine Quelle (Session 8).

---

## Fazit Session 8

Kompakte Session: ein Hoch-Prio-Punkt (stdlib-Drift) strukturell gelöst —
nicht zwei String-Literale synchronisiert, sondern eine gemeinsame Quelle
geschaffen. Zwei Fallstricke unterwegs (gofmt-Reflex, Env-Vorrang) haben
gezeigt, dass Tooling-Konvention und CLAUDE.md auseinanderliegen können;
CLAUDE.md gewinnt. Verifikation über beide Binaries (CLI + Server) statt
nur Unit-Tests hat die Drift wirklich als behoben bestätigt.

> "Zwei Quellen driften immer. Eine Quelle kann nicht driften."
> — Gerhard & Claude, Juni 2026

---

# Session 9 – 2026-06-16: Test-Netz vollendet (Todo #3)

**Autoren:** Gerhard Quell & Claude
**Branch:** main

---

## Ziel

Todo #3 abschließen: Primitiven-, Makro-Expansion- und parfunc/Channel-
Tests. Damit das Sicherheitsnetz von 36 auf volle Abdeckung der
eingebauten Funktionalität wachsen.

## Was haben wir gebaut?

| Test-Datei | Tests | Abdeckung |
|-----------|-------|-----------|
| `primitives_test.go` | 13 | mod/abs, Typ-Prädikate, Listen-Edges, Strings, fileio, gensym, error, memstats |
| `macros_test.go` | 12 | defmacro, uneval. Args, macroexpand, nested, hygiene, IsMacro |
| `concurrency_test.go` | 12 | parfunc (basic/order/timeout/error), buffered channels, lock |

**Tests gesamt:** 36 → 75 (Reader/Env 15 + Eval 21 + Primitive 13 + Makro 12 + Concurrency 12).
**Suite-Laufzeit:** 1.15s (parfunc-timeout-Test addiert ~1s).

## Was lief gut?

### Charakterisierungstest-Disziplin trug wieder
24 neue Tests, davon beim ersten Lauf 4 rot — alle IST-Funde, keine
Code-Bugs. Jeder Fund wurde als IST dokumentiert (nicht "repariert"),
genau wie bei Reader/Eval. Das Muster hält: falsche Erwartungen *sind*
der Wert.

### setq-vs-set!-Fund ist der wertvollste der Session
Beim swap-Makro-Hygiene-Test kam `(1 2)` statt `(2 1)` raus. Ursache:
`setq` (= `define` = `env.Set`) im inneren `let`-Body legt eine
Shadow-Variable an, statt die äußere zu updaten. `set!` (env.Update)
wäre nötig. Das ist eine latente Semantik-Entscheidung, die aus dem Code
nicht offensichtlich ist und jede/n Makro-AutorIn überrascht. Erst der
Charakterisierungstest machte sie sichtbar — und lieferte gleich den
Kontrast-Test (`set!`-Variante → `(2 1)`) als lebende Dokumentation.

### Deterministische Concurrency-Tests sind möglich
parfunc sammelt nach Expr-Index (Reihenfolge garantiert, unabhängig von
Ankunftszeit) — das macht es testbar ohne `time.Sleep`-Flakiness. Nur
der timeout-Test braucht echtes Timing (1s). Channel-Tests nur
buffered+sequenziell — unbuffered-send-ohne-receiver würde blockieren.
Concurrency testbar halten heißt: die Garantien des Systems ausnutzen,
nicht gegen seine Non-Determinism ankämpfen.

## Was nicht lief / Verbesserungspotenzial

### Go-Test-Caching täuschte über echten Zustand hinweg
`TestIsMacroGo` war im isolierten `-run TestMacro`-Lauf "grün", failte
aber bei `go test ./...`. Ursache: Cache-Hit von einem früheren Code-
Stand; erst der vollständige Lauf (cache invalidiert durch neue
concurrency_test.go) zeigte den echten Bug im Test (`defmacro` gibt
Atom "m" zurück, nicht das Makro — IsMacro muss auf das aus dem env
geholte Makro angewandt werden). **Lehre:** überraschende Test-Ergebnisse
mit `-count=1` oder `go clean -testcache` verifizieren. Cache lügt nicht,
aber er täuscht über aktuelle Konsistenz hinweg.

### CLI-stdin-Multi-Expr zeigte nicht alle Ergebnisse
Beim manuellen swap-Verifizieren via `printf '...\n' | ./golisp` erschien
nur die `defmacro`-Rückgabe, nicht das `let`-Ergebnis. Mehrere Ausdrücke
über stdin werden ausgewertet, aber die Ausgabe-Strategie bei mehreren
Ergebnissen ist unklar/inkonsistent. Hätte mich auf `go test` verlassen
sollen statt CLI-Piping zu debuggen. CLI-Multi-Expr-Verhalten ist ein
eigenes Untersuchungsthema.

### Hygiene-Test war ursprünglich falsch konstruiert
Der erste `TestMacroHygieneWithGensym` wollte gensym-vs-kein-gensym
demonstrieren, aber der swap bricht nicht an gensym, sondern an der
setq-Semantik. Ich musste den Test umgestalten: statt "Hygiene zeigen"
→ "setq-Shadowing dokumentieren + set!-Kontrast + gensym-Unique". Lehre:
Tests müssen das Verhalten dokumentieren das *ist*, nicht das, das man
*demonstrieren wollte*. Wenn der Test nicht das zeigt was ich will, ist
meine Hypothese falsch — nicht der Code.

## Schlüssel-Erkenntnisse

### Charakterisierungstests als latente Semantik-Dokumentation
Der setq-vs-set!-Fund ist kein Bug-Fund, sondern ein Verhaltens-Fund:
das System verhält sich deterministisch, aber die Determinismus-Regel
("setq = Set im current-env, shadowed bei scope-Tiefe") ist nirgends
dokumentiert. Der Test ist jetzt die Dokumentation. Wer später fragt
"warum ändert mein swap-Makro nichts?" findet den Test und die Antwort.

### eq? = eq (Pointer) bestätigt Type-System-Konsistenz
`eq?` und `eq` sind beide Pointer-Vergleich. Zwei `'foo`-Instanzen sind
nicht `eq?`. Das ist konsistent mit der Singleton-Nil-Optimierung
(`eq (list) (list)` = `t`, weil identische nilCell). Das Type-System
ist pointer-first — wer strukturelle Gleichheit will, muss `equal?`
nutzen. Diese Konsistenz wäre ohne Tests nur schwer zu vertrauen.

### Concurrency-Testbarkeit als Architektur-Validierung
Dass parfunc deterministisch testbar ist (idx-geordnete Ergebnisse),
ist kein Zufall — es ist eine bewusste Design-Entscheidung in
`evalParfunc`: `gathered[r.idx] = r.val` sortiert nach Index, nicht nach
Ankunftszeit. Das macht das Feature testbar. Architekturen, die
Ankunfts-Reihenfolge zurückgeben würden, wären untestbar gewesen.
Testbarkeit ist hier eine emergente Eigenschaft guten Designs.

---

## IST-Funde (kumuliert über alle Sessions)

| Fund | Wo | Status |
|------|----|---------|
| `Cell.String()`: NIL-Cell → `"()"`, nil-Ptr → `"NIL"` | types.go | dokumentiert |
| Backslash außerhalb String = Symbol | reader.go | dokumentiert |
| Dotted-pair-Reader blind nach cdr | reader.go | dokumentiert (Todo #7) |
| Stille Typkoersion `(+ 1 "x")`=1 | primitives.go | dokumentiert (Fix offen) |
| `(- 5)`=5 (kein unäres Minus) | primitives.go | dokumentiert |
| `(if)`=`()` (permissive Syntax) | eval.go | dokumentiert |
| `eq?` = Pointer wie `eq` | primitives.go | dokumentiert |
| `atom? '()` = `t` (NIL ≠ LIST-Typ) | primitives.go | dokumentiert |
| `file-write`/`file-append` geben Pfad zurück | fileio.go | dokumentiert (API-Inkonsistenz) |
| `setq` shadowed in innerem let, `set!` nötig | eval.go | dokumentiert (Makro-Autor-Falle) |
| `(parfunc r)` ohne Expr setzt `r` nicht | eval_control.go | dokumentiert (Mini-Bug) |
| stdlib `max`/`min` nur 2-args | stdlib.lisp | dokumentiert (Arity undokumentiert) |

---

## Offene Punkte (nach dieser Session)

- [ ] **Todo #5-7:** Duplikat-Bereinigung (sliceToCell/isTruthy/countParens),
  sigoREST-Konfig (Default-Modell, Host-Env), Kleinigkeiten (Tabs,
  pg-Conn-Typ, dotted-pair-Check, nil-Prüfungen).
- [ ] **Latente Bugs fixen:** Stille Typkoersion in primitives.go;
  parfunc-Empty-Setzt-r-nicht; stdlib max/min variadisch machen.
- [x] ~~Todo #3 Testinfrastruktur~~ → 75 Tests, vollständige Primitive/
  Makro/Concurrency-Abdeckung (Session 9).

---

## Fazit Session 9

Dritte Test-Session, die das Sicherheitsnetz von 36 auf 75 Tests
verdoppelte. Der wertvollste Fund war kein Bug, sondern eine latente
Semantik-Regel: `setq` shadowed in inneren Scopes, `set!` updatet. Das
ist genau der Wert von Charakterisierungstests — sie dokumentieren das
Verhalten das *ist*, einschließlich der Subtilitäten, die aus dem Code
allein nicht ersichtlich sind. GoLisp hat jetzt ein Test-Netz, das nicht
nur Refactor-Sicherheit bietet, sondern als lebende Verhaltens-Doku
dient. Todo #3 vollständig erledigt.

> "Tests dokumentieren nicht, was der Code tun soll – sie dokumentieren,
> was er wirklich tut. Darin liegt ihr wertvollster Fund."
> — Gerhard & Claude, Juni 2026

---

# Session 10 – 2026-06-16: Latente Bugs gefixt

**Autoren:** Gerhard Quell & Claude
**Branch:** main

---

## Ziel

Die in Sessions 7-9 dokumentierten latenten Bugs beheben. Drei Kandidaten
aus der kumulierten IST-Funde-Tabelle:
1. `(parfunc r)` ohne Expr setzt `r` nicht im env (Mini-Bug)
2. Stille Typkoersion `(+ 1 "x")`=1 (stiller Datenverlust)
3. stdlib `max`/`min` nur 2-args, nicht variadisch

## Was haben wir gemacht?

### Bug 1: parfunc-empty (safe fix)
`evalParfunc` sprang bei leerer exprList früh per `return MakeNil()` ab –
*vor* `env.Set(resultName, ...)`. Fix: `env.Set(resultName, MakeNil())`
vor das return ziehen. `r` ist jetzt gebunden. Backwards-kompatibel.

### Bug 3: max/min variadisch (low-risk)
stdlib `max`/`min` waren `(defun max (a b) ...)` – nur 2 Argumente.
CL-Variante ist variadisch. Fix via `&rest` + `reduce`:
`(defun max (a &rest rest) (reduce (lambda (x y) (if (>= x y) x y)) a rest))`.
Backwards-kompatibel: `(max 3 7)` funktioniert weiter, `(max 3 7 2)`=7 neu.

### Bug 2: Stille Typkoersion (breaking, Design-Entscheidung)
Arithmetik-Primitive (`+,-,*,/,mod,abs`) und Vergleiche (`=,<,>,>=,<=`)
griffen direkt auf `.Num` zu. Strings haben `Num=0`, wurden still addiert:
`(+ 1 "x")`=1, `(= "a" "a")`=t. Stiller Datenverlust.

**Design-Entscheidung via AskUserQuestion:** drei Optionen (Strict /
Lax belassen / Nur Vergleiche). Gerhard wählte **Strict**.

Fix: zentrale `checkNumbers(name, args)`-Hilfsfunktion in primitives.go,
die alle args auf `NUMBER`-Typ prüft und `fmt.Errorf("%s: Zahl erwartet,
got %s", name, a)` wirft. Eingebaut in alle 11 betroffenen Primitive.
Vergleiche mit strict gemacht für Konsistenz (sonst `(+ 1 "x")`→error
aber `(= 1 "x")`→still `()`).

**Breaking:** Programme die auf stiller Koersion vertrauten, brechen
jetzt. Aber: nur 1 Test failte (der den Bug dokumentiert hatte), keine
stdlib-interne Nutzung brach (`length`, `iota`, `max`, `gcd` reichen
Zahlen sauber weiter). Confidence aus 75-Test-Netz.

## Was lief gut?

### Test-Netz als Confidence-Quelle für breaking Change
Die strict-Typing-Änderung ist breaking. Aber das 75-Test-Netz deckte
genau ab, was kaputtgehen könnte: nur `TestEvalSilentTypeCoercion` failte
(der Bug war dort als IST dokumentiert). Kein stdlib-Pfad brach. Ohne das
Netz wäre ein breaking Change ein Glücksspiel – mit Netz eine berechnete
Entscheidung. Genau der Compound-Wert der Test-Investition aus Session 7+9.

### Design-Entscheidung eingeholt statt geraten
Bei Bug 2 (breaking) nicht einfach "ich mache strict" geraten, sondern
per AskUserQuestion drei Optionen mit Preview präsentiert. Gerhard
entschied. Breaking Changes gehören dem Nutzer, nicht dem Werkzeug.

### Kumulierte IST-Funde-Tabelle als Arbeits-Backlog
Die in Session 9 eingeführte Tabelle diente hier direkt als
Bug-Backlog: drei Einträge mit "dokumentiert (Fix offen)" wurden
abgearbeitet. Ohne die Tabelle wären die Bugs über Sessions verstreut
und einzeln mühsam wiederzufinden. Dokumentation als TODO-Liste.

## Was nicht lief / Verbesserungspotenzial

### test_infra-Discovery: evalStr lädt keine stdlib
Beim Testen von Bug 3 (max/min) fiel auf: der Test-Helper `evalStr`
nutzt `BaseEnv()` ohne `LoadStdlib` – stdlib-Funktionen (max, min, iota)
sind in Unit-Tests nicht testbar. Bug 3 musste via CLI-Smoke verifiziert
werden (main.go lädt stdlib). Lücke: kein formeller Test für
stdlib-Funktionen. Kandidat für später: `evalStd(src)`-Helper mit
LoadStdlib, oder eigenes stdlib_test.go.

### Vergleiche-strict war Ausweitung der Wahl
Gerhard wählte "Strict" im Arithmetik-Kontext. Ich habe die Vergleiche
(`=,<,>`) *zusätzlich* strict gemacht, mit Begründung "Konsistenz". Das
ist eine Interpretation seiner Wahl. Hätte ich die Vergleiche separat
nachfragen sollen? Wahrscheinlich ja – es war eine Ausweitung. Hat sich
als richtig erwiesen (kein Widerspruch), aber das Prinzip "breaking
Changes gehören dem Nutzer" gilt auch für Ausweitungen.

## Schlüssel-Erkenntnisse

### Tests ermöglichen breaking Changes mit Confidence
Das ist die Umkehrung der üblichen "Tests verhindern Regression"-Story:
Tests *ermöglichen* mutige Changes, weil sie aufzeigen, was genau bricht.
Strict typing ist breaking – aber mit 75 Tests war es eine berechnete
Entscheidung, kein Sprung ins Dunkle. Der Wert eines Test-Netzes wächst
nicht nur mit der Abdeckung, sondern mit der *Confidence*, die es für
kommende Refactorings/Bugfixes bietet.

### `checkNumbers` als zentrale Wächter-Funktion
Statt in jeder Primitive inline `if a.Type != NUMBER` zu schreiben, eine
Hilfsfunktion mit Operator-Namen. Vorteil: einheitliche Fehlermeldung
("+ : Zahl erwartet, got ..."), wartbar an einer Stelle, Muster für
künftige Primitive etabliert. Architektur-Gewinn aus dem Bug: die Lösung
ist strukturierter als der Bug-Zustand.

### Breaking-Change-Kommunikation ist separater Schritt
Strict typing bricht Programme, die (absichtlich/unabsichtlich) auf
stiller Koersion vertrauten. Tests dokumentieren das neue Verhalten, aber
Nutzer-Communication (CHANGELOG, Release-Note) ist ein separater Schritt,
den die Tests nicht ersetzen. U-Boot-Philosophie mildert (reift in Ruhe),
aber beim "Zeigen" erwähnenswert.

---

## IST-Funde-Status (aktualisiert)

| Fund | Wo | Status |
|------|----|---------|
| `Cell.String()`: NIL→`()`, nil-Ptr→`"NIL"` | types.go | dokumentiert |
| Backslash = Symbol | reader.go | dokumentiert |
| Dotted-pair-Reader blind | reader.go | offen (Todo #7) |
| ~~Stille Typkoersion~~ | primitives.go | **gefixt (strict)** |
| `(- 5)`=5 (kein unäres Minus) | primitives.go | IST, ok |
| `(if)`=`()` (permissive) | eval.go | IST, ok |
| `eq?`=Pointer | primitives.go | dokumentiert |
| `atom? '()`=t | primitives.go | dokumentiert |
| `file-write`/`-append` geben Pfad | fileio.go | dokumentiert (API) |
| `setq` shadowed in innerem let | eval.go | dokumentiert |
| ~~`(parfunc r)` ohne Expr setzt r nicht~~ | eval_control.go | **gefixt** |
| ~~stdlib `max`/`min` nur 2-args~~ | stdlib.lisp | **gefixt (variadisch)** |

3 von 12 dokumentierten Funds gefixt. 6 bleiben als gewolltes IST, 1 offen
(dotted-pair, Todo #7), 2 als API-Inkonsistenz dokumentiert.

---

## Offene Punkte (nach dieser Session)

- [ ] **Todo #5-7:** Duplikat-Bereinigung, sigoREST-Konfig, Kleinigkeiten.
- [ ] **test_infra:** `evalStd(src)`-Helper oder stdlib_test.go –
  stdlib-Funktionen formell testbar machen.
- [ ] **Breaking-Change-Note:** strict typing für künftiges "Release"
  dokumentieren (CHANGELOG o.ä.).
- [x] ~~3 latente Bugs~~ → parfunc-empty, Typkoersion (strict), max/min
  variadisch gefixt (Session 10).

---

## Fazit Session 10

Kompakte Bug-Fix-Session: drei dokumentierte latente Bugs abgearbeitet,
davon eine breaking Design-Entscheidung (strict typing) per
AskUserQuestion mit Gerhard geklärt. Das 75-Test-Netz machte den
breaking Change zu einer berechneten Entscheidung statt einem Glücksspiel
– nur der Test, der den Bug dokumentiert hatte, failte. Kumulierte
IST-Funde-Tabelle diente als direktes Bug-Backlog. Drei Funds von zwölf
gefixt, die Struktur (checkNumbers) ist besser als der Bug-Zustand.

> "Tests verhindern nicht nur Regression – sie ermöglichen mutige
>  Changes. Confidence ist der wahre Compound-Wert eines Test-Netzes."
> — Gerhard & Claude, Juni 2026

---

# Session 11 – 2026-06-16: Aufräumen & Konfig (Todo #5, #6, #7.3)

**Autoren:** Gerhard Quell & Claude
**Branch:** main
**Tagesabschluss-Retro** (5. Session des Tages nach 7-10)

---

## Ziel

Nach Test-Netz (7+9), stdlib-Zentralisierung (8) und Bugfixes (10) die
niedrig-prioren Todos abarbeiten: #5 Code-Duplikation, #6 sigoREST-Konfig,
#7.3 dotted-pair-Reader-Check. Den Tag sauber abschließen.

## Was haben wir gemacht?

### Todo #5 – Code-Duplikation bereinigt
Drei byteweise identische Helper-Duplikate entfernt: unexported
`isTruthy`/`sliceToCell`/`cellToSlice` in eval_core.go waren Schatten der
exportierten `IsTruthy`/`SliceToCell`/`CellToSlice` in types_helpers.go.
13 Aufrufstellen über 5 Files auf exported-Versionen umgestellt.
`readline.go.v2` (dokumentierter Fallback) nach `docs/` archiviert.
`countParens` existierte gar nicht (Todo veraltet).

### Todo #6 – sigoREST-Konfig via Env-Vars
Default-Modell war `ollama-gemma3-4b` — **nicht mehr in Live-Liste** →
`(sigo "prompt")` ohne Modell-Arg failte (verdeckter Bug). Neuer Default
`gem25-flt` (live, verifiziert). `GOLISP_SIGO_HOST`/`GOLISP_SIGO_MODEL`
env beim Start via `init()`. CLAUDE.md dokumentiert.

### Todo #7.3 – dotted-pair-Reader-Check
`readRest` konsumierte nach `(a . b)` das `)` blind per `r.next()` —
Müll wie `(a . b x)` wurde still akzeptiert. Jetzt `peek`+Prüfung, Fehler
bei Nicht-`)`. Session-7-Fund (damals als IST dokumentiert) jetzt Bugfix.

### Zusätzlich: sigoREST-Zugang verifiziert + CLAUDE.md-Modelle aktualisiert
Live-Check: sigoREST PID 1757, Ports 9080/9443, `(sigo "test" "gem25-flt")`
→ "OK". CLAUDE.md-Modelltabelle von 13 → ~30 Einträge ergänzt (cl47-o,
cl48-o, gem35-f etc.), als "runtime-dynamisch, siehe (sigo-models)"
markiert. Memory `sigorest_models.md` neu erstellt.

## Tagesbilanz

| Metrik | Wert |
|--------|------|
| Commits heute | 12 (6 Code + 5 Retro/Doc + 1 Config) |
| Sessions dokumentiert | 5 (Session 7-11) |
| Todos erledigt | #1, #2, #3, #4, #5, #6, #7.3 |
| Tests | 2 → 76 |
| eval.go | 1003 Zeilen → 6 Module |
| Latente Bugs gefixt | 4 (Typkoersion, parfunc-empty, max/min, dotted-pair) |
| Stdlib-Quellen | 2 (Drift) → 1 (LoadStdlib) |
| Duplikate entfernt | 3 Helper + 1 Backup-File |

## Was lief gut?

### Test-Netz als durchgehender Compound-Wert
Jede Session nach Session 7 profitierte vom Test-Netz: Split lief grün
beim ersten Versuch, stdlib-Zentralisierung verifiziert über beide
Binaries, Bugfixes (breaking strict typing!) mit Confidence, Dedup über
5 Files ohne Runtime-Regression, dotted-pair-Fix sofort gesichert. Der
Invest in 76 Tests zahlte sich bei *jedem* der 12 Commits aus. Das ist
der Definition von Compound-Value.

### Kumulierte IST-Funde-Tabelle als Arbeits-Backlog gereift
Session 9 eingeführt, Session 10 als Bug-Backlog genutzt (3 gefixt),
Session 11 setzte den 4. Fund (dotted-pair) um. Die Tabelle ist jetzt
eine *Trackbare Verhaltens-Spezifikation* — jeder Eintrag hat Status
(dokumentiert/gefixt/IST-ok). Was als Beobachtung begann, wurde zu
verlässlicher Projekt-Doku.

### Code/Doc/Retro-Rhythmus als built-in Disziplin
12 Commits, aber kein einziges Mal "8 Commits am Stück durchziehen".
Jeder Code-Commit hatte einen klaren Fokus, jeder Retro-Commit erzwang
Reflexion dazwischen. Commit-Rhythmus als built-in Retrospektive — man
kann nicht mutig refactor-ieren ohne zwischendurch zu fragen "was lief
gut, was nicht".

### Config-Feature deckte verdeckten Bug auf
Todo #6 war als "Config verbessern" deklariert, entpuppte sich als
Bugfix: der Default `ollama-gemma3-4b` war tot. Wer nur "Config
hinzufügen" wollte, hätte den toten Default übersehen. Todo-Liste
sorgfältig lesen = Bug-Quelle erkennen.

## Was nicht lief / Verbesserungspotenzial

### Sehr langer Tag, 5 Sessions — Erschöpfungsrisiko
12 Commits, 5 Retros in einem Tag ist außergewöhnlich viel. Späte
Sessions (10, 11) liefen noch diszipliniert, aber das Risiko von
Qualitätsverlust in Session 12+ wäre real. **Lehre:** bei langen Tagen
bewusst Pausen machen oder ab Session 8-9 nur noch niedrig-risk Tasks.
Heute ging es gut weil die Test-Infra jeden Schritt auffing.

### gofmt-vs-2-Space-Konflikt bleibt ungelöst (Todo #7 Rest)
Projekt nutzt bewusst 2-Space (CLAUDE.md), gofmt will tabs. `gofmt -l`
listet alle Files. Keine Lösung gefunden — nur vermieden (nicht gofmt
anwenden). Offen: pre-commit-Hook der 2-Space erzwingt, oder CLAUDE.md
explizit "gofmt ignorieren" dokumentieren. Unschön, aber nicht blockierend.

### verwaiste Memory-Files (user_profile, project_status) unentdeckt
MEMORY.md verweist auf 3 Files, nur sigorest_models existierte (heute
erstellt). user_profile und project_status fehlen — project_status wäre
stark veraltet ("eval.go aufteilen offen" — völlig falsch nach heute).
Lücke für nächste Session.

## Schlüssel-Erkenntnisse des Tages

### 1. Sicherheitsnetz zuerst, dann operieren
Session 7s Prinzip ("Netz vor Trampolin-OP") trug den ganzen Tag. Die
Reihenfolge certs → Tests → Split → Bugs → Config → Dedup war nicht
Zufall sondern Risiko-Minimierung: jeder Schritt stand auf dem vorigen.

### 2. Eine Quelle schlägt Synchronisation
stdlib-Zentralisierung (Session 8): eine `LoadStdlib` statt zwei
String-Literale. Helper-Dedup (Session 11): ein `IsTruthy` statt
Schatten-Duplikat. Derselbe Architektur-Gedanke, zweimal angewandt:
strukturelle Unmöglichkeit von Drift/Duplikat statt Disziplin.

### 3. Charakterisierungstests als Bugfix-Backlog
Sessions 7+9 dokumentierten IST-Verhalten (4 latente Bugs). Sessions 10+11
fixten sie. Der Zyklus Bug-finden → als IST festhalten → später gezielt
fixen → als SOLL sichern ist reif geworden. Tests als lebende Verhaltens-
Doku, die in ausführbare Specs reift.

### 4. Config-Aufgaben verbergen oft Bugs
Todo #6 "Config verbessern" → toter Default-Modell-Bug. Todo #2 "stdlib
zentralisieren" → Drift-Bug. Wer Config-Todos liest und "nur Settings"
denkt, verpasst die versteckten Defekte. Implizite Annahmen (Default
existiert, zwei Quellen sind gleich) immer verifizieren.

---

## Offene Punkte (nach Session 11)

- [ ] **Todo #7 Rest:** Einrückung (gofmt-vs-2-Space-Konflikt ungelöst),
  pg-Conn-Typ (postgres.go), nil-Prüfungen in eval-Helfern.
- [ ] **verwaiste Memory-Files:** user_profile.md, project_status.md
  erstellen/aktualisieren (project_status stark veraltet).
- [ ] **test_infra:** `evalStd(src)`-Helper oder stdlib_test.go.
- [ ] **Breaking-Change-Note:** strict typing für künftiges Release.

---

## Fazit Session 11 & Tagesabschluss

Kompakte Aufräum-Session die drei niedrig-prioren Todos abarbeitete,
davon einer (#6) wieder einen verdeckten Bug aufdeckte (toter Default-
Modell). Der Tag endet mit 12 Commits, 5 dokumentierten Sessions, 76
Tests, 4 gefixten Bugs — und einem GoLisp das strukturell gesünder ist
als morgens: weniger Duplikate, eine stdlib-Quelle, konfigurierbarer
sigoREST, striktere Typisierung, sauberer Reader.

> "Ein Tag der das System nicht funktional erweiterte, aber strukturell
>  heilte. Manchmal ist Aufräumen die wertvollste Feature-Arbeit."
> — Gerhard & Claude, Juni 2026

---

# Session 12 – 2026-06-21: SWANK/SLIME-Integration zum Laufen gebracht

## Ziel

Todo #1 validieren: Der in Session- predecessors gebaute Swank-Server
(`lib/swank/`, Commits 116f28b / 7aa8c8d) war nie gegen echte Emacs-SLIME-
Session getestet. Ziel: `slime-connect` funktioniert, REPL evaluiert.

## Was haben wir gebaut / gefixt?

Drei Commits, sechs behobene Probleme, iterativ gegen SLIME v2.32
(via quicklisp) erarbeitet.

| Commit | Inhalt |
|--------|--------|
| `6bd171d` | fix(swank): persistenter `bufio.Reader` pro Connection — Pipelining-Bug |
| `f499ce0` | fix(eval): `(eval form)` im globalen Env (CL-Semantik) — sonst `defun` aus REPL verloren |
| `f1e6638` | feat(swank): SLIME-kompatible Handler |

Behobene Probleme in Reihenfolge des Auftretens:

1. **bufio-Pipelining-Bug.** `readFrame` erzeugte pro Call neuen
   `bufio.Reader`; vorausgelesene Frame-Bytes wurden mit dem verworfenen
   Reader gelöscht. Schon beim Code-Lesen als Verdacht notiert, dann mit
   3 gepipelinten Frames bewiesen (1/3 Responses → 3/3 nach Fix).
2. **`swank-repl:`-Prefix fehlt.** SLIME sendet `swank-repl:create-repl` /
   `swank-repl:listener-eval`, nicht `swank:`. Bestehende Handler matchten
   nie. Erst durch `>>`/`<<`-Server-Trace sichtbar.
3. **`:abort` auf unbekannte Ops.** Default-Fall warf `:abort` → "Synchronous
   Lisp Evaluation aborted". SLIME-Contribs rufen beim Connect diverse
   Init-Funktionen. Fix: graceful `(:ok ())`.
4. **`listener-eval` Return-Format.** `(:ok "54")` (String) → SLIME will
   Liste → `listp`-Error. Richtig: `(:write-string "<wert>\n" :repl-result)`
   + `(:ok nil)`. Aus `swank-repl.lisp` `send-repl-results-to-emacs`
   abgelesen.
5. **`autodoc` destructure.** SLIME `(cl-destructuring-bind (doc
   &optional cache-p) doc)`. Leere Liste → 0 Args. `(:ok (nil nil))` →
   `doc=nil` → `insert nil` → `char-or-string-p`. Endgültig
   `(:ok (:not-available nil))` — das Keyword, das SLIME explizit abfragt.
6. **`defun` verschwindet.** `(fib 3)` nach `(defun fib ...)` → "unbekannt
   Symbol". `(eval form)` nutzte dynamisches Env; in der Lambda-Kette
   `swank-dispatch → handle-emacs-rex → listener-eval` ist das ein Child-Env,
   `defun` definierte lokal. Fix: `Env.Root()`, CL-Semantik. Core-Change.

## Was lief gut?

- **Iterativ gegen das echte System.** Sechs Iterationen je eine Code-
  Änderung + Reconnect. Jede SLIME-Fehlermeldung war präziser Fingerzeig.
  Schneller als jede Voraus-Planung.
- **Verdacht先行 (suspicion-first).** bufio-Bug schon beim ersten Lesen von
  `framing.go` vermutet, explizit getestet — nicht erst auf Symptom gewartet.
  5 min vom Verdacht zum Beweis.
- **Gegenseite lesen.** Statt Protokoll zu raten, in `swank-repl.lisp` und
  `slime-autodoc.el` gelesen, was SLIME tatsächlich destrukturiert. Ein
  `cl-destructuring-bind` löste Iteration 5 sofort. Mehr wert als jede Spec.
- **双向 Trace früh.** Go-Errors sahen still aus; Lisp-seitige `:abort`-
  Returns standen nicht im Log. `>>`/`<<`-Trace eingebaut → sah sofort welche
  Ops reinkamen. Decisive für Iteration 2-5. (Leider erst Iteration 2, nicht
  1 — siehe unten.)
- **Core vs. swank im Commit getrennt.** eval-Global-Semantik ist core
  change, eigener Commit mit eigener Begründung. Nicht im swank-Commit
  versteckt.

## Was nicht lief / Verbesserungspotenzial

- **Synthetischer Testclient zu naiv.** Erste `swankc2.go` las 1 Response
  pro Message, aber `create-repl` sendet 2 Events. Output-Verschiebung sah
  aus wie Server-Bug, war Client-Bug. Verwirrend, bis pipelined Test den
  echten Bug zeigte.
- **Klammerfehler im Lisp-Edit.** Eine Edit ließ `handle-emacs-rex`-Defun
  offen → Tests rot ("fehlendes )"). Go-Tests fingen's sofort, aber: GoLisp
  hat keinen Inline-Balancing-Check; `go test` nach jeder Lisp-Edit ist der
  einzige Rettungsanker. Fehleranfällig.
- **Trace zu spät.** Erst in Iteration 2 eingebaut. Hätte von Anfang an
  sein sollen — Iteration 1 war im Dunkeln.
- **Sandbox vs. Background-Prozesse.** `&`-Jobs wurden vom Harness-Wrapper
  gekillt (Exit 144). Mehrere Anläufe bis `run_in_background: true`
  zuverlässig lief. Zeit am Tooling statt am Fachproblem.
- **Punkt 6 spät erkannt.** Dass `defun` nicht persistiert, zeigte sich
  erst als der REPL scheinbar funktionierte. Synthetische Tests prüften nur
  Einzelexpr, nicht `defun` + späteren Call über dieselbe Connection.

## Schlüssel-Erkenntnisse

1. **Gegenseite lesen, nicht raten.** Bei Protokoll-Integration den Client-
   Source lesen. `cl-destructuring-bind` und `send-repl-results-to-emacs`
   sagen mehr als jede Spec.
2. **Verdacht → Test → Fix.** Beim Code-Lesen gefundene Bug-Verdachte direkt
   testen. bufio-Bug in 5 min bewiesen statt 5 Iterationen symptomgetrieben.
3. **Trace früh,双向.** RPC-Systeme brauchen in+out-Trace ab Iteration 1,
   nicht ab Iteration 2. Billig einzubauen, unbezahlbar im Debugging.
4. **Integrationstests testen mehr als die Integration.** Punkt 6 (eval-Env)
   ist ein core-Semantik-Bug, der nur durch den REPL-Integrationstest
   sichtbar wurde. Protokoll-Tests sind core-Tests in Verkleidung.
5. **Synthetische Tests müssen echtes Verhalten modellieren.** Pipelining,
   Multi-Event-Responses, zustandsbehaftete Connections — sonst testen sie
   nicht was SLIME tut.
6. **CL-Semantik als Kompass.** Wenn GoLisp-Verhalten unklar ist, sagt
   Common-Lisp-Spezifikation was richtig ist (`eval` global). Hat Punkt 6
   sofort auf die Lösung gelenkt.

## IST-Funde (Session 12)

- `Env.Set` schreibt nur lokal — kein Walk-up. `defun`/`define`/`setq` sind
  in Lambda-Bodies lokal, nicht global. Per Design, aber CL-unüblich.
  Komplett global machen würde `let`-lokale Defines brechen. Status: nur
  `eval` geht über Root, `defun` direkt bleibt lokal. Bewusst so belassen.

## Offene Punkte (nach dieser Session)

- Weitere SWANK-Methoden: `complete-symbol` (Tab-Completion),
  `describe-symbol` / `arglist-for-echo-area`, `macroexpand`,
  `compile-string`, `load-file`.
- `listener-eval`: mehrere Formen pro String (derzeit nur erste via `read`).
- slime-tramp für Emacs (Todo #2).
- CLAUDE.md um eval-Global-Semantik + swank-REPL-Status ergänzen.
- GoLisp-Lisp-Edits ohne Balancing-Check bleiben fehleranfällig — evtl.
  Reader-Warnung bei unausgeglichenen Klammern in `load`/`read`.

## Fazit Session 12

Vom "MVP steht aber ungetestet" zum "REPL mit Output, Rekursion und
persistierenden Definitionen" in einer Session. Sechs Iterationen, drei
Commits, davon ein core-Semantik-Fix der nur durch den Integrationstest
sichtbar wurde. Der Swank-Server ist jetzt eine echte Emacs-Entwicklungsum-
gebung für GoLisp — nicht mehr nur Gerüst.

Lehrreichster Moment: dass der scheinbare Protokoll-Bug (`defun` verschwindet)
ein core-eval-Bug war. Integrationstests sind core-Tests in Verkleidung.

> "Sechs Iterationen gegen das echte System — jede SLIME-Fehlermeldung ein
>  präziserer Lehrer als jede Spec. Am Ende war der letzte Bug kein
>  swank-Bug, sondern ein core-Bug, den nur der REPL-Test aufdeckte."
> — Gerhard & Claude, 21. Juni 2026
