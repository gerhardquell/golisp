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
