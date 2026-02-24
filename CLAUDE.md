# GoLisp – CLAUDE.md

## Projekt-Übersicht
GoLisp ist ein Lisp-Interpreter in Go mit nativer KI-Anbindung (sigoREST).
Ziel: Ein selbsterweiterndes System das Goroutinen, Channels und KI-Calls
als eingebaute Lisp-Primitiven beherrscht.

**Autor:** Gerhard Quell – gquell@skequell.de
**CoAutor:** claude sonnet 4.6
**Modul:** `golisp`
**Sprache:** deutsch
---

## Dateistruktur

```
golisp/
  main.go              REPL + Testmodus (-t) + Datei-Loader
  lib/
    types.go           Cell-Datenstruktur (LispType, Cons, MakeAtom...)
    reader.go          Parser: String → Cell-Baum (NewReader, Read)
    env.go             Umgebung: Get, Set, Update, Symbols (verkettete Scopes)
    eval.go            Herzstück: Eval, Spezialformen, defmacro, parfunc
    primitives.go      Eingebaute Funktionen + BaseEnv()
    stringfuncs.go     String-Primitiven (RegisterStringFuncs)
    goroutine.go       parfunc, chan-make/send/recv, lock-make
    fileio.go          file-write, file-append, file-read, file-exists?, file-delete
    sigorest.go        sigo, sigo-models, sigo-host (HTTP zu sigoREST)
    readline.go        REPL: go-prompt, Syntax-Highlighting, History, Multiline
    env_test.go        Go-Tests für Env.Symbols()
```

---

## Coding-Konventionen

- **Sprache:** Go für den Kern, Lisp für Erweiterungen
- **Einrückung:** 2 Spaces, keine Tabs
- **Dateinamen:** camelCase (außer main.go)
- **Kommentare:** sparsam, sprechende Namen bevorzugt
- **Dateigröße:** max 300 Zeilen, ab 500 aufteilen
- **Datei-Header:** immer mit Autor, CoAutor, Copyright, Erstellt (YYYYMMDD)
- **Fehler:** `fmt.Errorf("funktionsname: beschreibung")`

### Spezialformen vs. Primitiven
- Braucht die Funktion Zugriff auf `env`? → Spezialform in `eval.go`
- Reine Berechnung ohne env? → Primitiv in `primitives.go`
- Gruppe verwandter Primitiven → eigene Datei mit `RegisterXxx(env *Env)`
- Neue Primitiven immer in `BaseEnv()` registrieren

---

## Architektur

### Cell – die Grundstruktur
```go
type Cell struct {
  Type LispType        // ATOM, NUMBER, STRING, LIST, FUNC, MACRO, NIL
  Val  string          // für ATOM und STRING
  Num  float64         // für NUMBER
  Car  *Cell           // Kopf einer Liste
  Cdr  *Cell           // Rest einer Liste
  Fn   func([]*Cell) (*Cell, error)  // für FUNC
  Env  interface{}     // für Lambda-Closures (*Env) und Go-Objekte
}
```

### Eval-Reihenfolge in evalList
```
1. Spezialformen prüfen (quote, if, define, defun, lambda,
   let, begin, set!, defmacro, mapcar, load, and, or, not,
   parfunc, lock, eval, catch, while, do, quasiquote, cond)
2. Makro-Expansion (MACRO-Typ → expand → Eval des Ergebnisses)
3. Normale Anwendung: Funktion auswerten → Argumente auswerten → apply
```

### TCO – Trampolin in Eval()
Lambda-Calls und alle Tail-Spezialformen (`if`, `begin`, `let`, `cond`)
setzen `expr`/`env` und machen `continue` im `for {}`-Loop — kein neuer
Stack-Frame, O(1) Stack für beliebig tiefe Tail-Rekursion.

### Lambda-Struktur
```go
// Lambda/Closure wird als Cell{Type:LIST} gespeichert:
Cell{Type: LIST, Car: params, Cdr: body, Env: closureEnv}
// Makro: identisch aber Type: MACRO
```

### Multi-Body: wrapBegin
`defun`, `lambda`, `defmacro` akzeptieren mehrere Body-Ausdrücke.
`wrapBegin(exprs)` wrappet sie zur Definitionszeit in `(begin ...)`.
Einzelner Ausdruck → direkt, kein Overhead.

---

## Implementierte Features

### Spezialformen
`quote` `if` `define` `defun` `lambda` `let` `begin` `set!`
`defmacro` `mapcar` `load` `and` `or` `not` `parfunc` `lock` `eval`
`catch` `while` `do` `quasiquote` `cond`

### Eingebaute Funktionen
**Arithmetik:** `+` `-` `*` `/`
**Vergleiche:** `=` `<` `>` `>=` `<=`
**Listen:** `car` `cdr` `cons` `atom` `null` `list` `apply` `equal?`
**I/O:** `print` `println` `read`
**String:** `string-length` `string-append` `substring`
  `string-upcase` `string-downcase` `string->number` `number->string`
**Fehler:** `error` `catch`
**Makro-Hilfe:** `gensym`
**Datei:** `file-write` `file-append` `file-read` `file-exists?` `file-delete`
**Nebenläufigkeit:** `chan-make` `chan-send` `chan-recv` `lock-make`
**KI:** `sigo` `sigo-models` `sigo-host`

### REPL (readline.go)
- **Syntax-Highlighting:** Klammern nach Tiefe eingefärbt (6 Farben, fett)
  Strings grün · Kommentare grau · Quote-Zeichen gelb
- **Multi-line:** Enter bei offenem Ausdruck → automatische Einrückung
- **History:** persistent `~/.golisp_history` (500 Einträge)
- **Library:** `github.com/elk-language/go-prompt`

---

## sigoREST Anbindung

GoLisp spricht mit dem sigoREST-Server:
```
Host: http://127.0.0.1:9080 (Default)
Endpoint: POST /v1/chat/completions
```

Verfügbare Modell-Shortcodes: `claude-h` `gemini-p` `gpt41`
und alle lokalen Ollama-Modelle (z.B. `ollama-gemma3-4b`)

### Das selbsterweiternde Muster
```lisp
; KI schreibt Code → GoLisp führt ihn aus
(eval (read (sigo "schreibe (defun fib (n) ...)" "claude-h")))
(fib 10)

; Ensemble: 3 KIs parallel
(parfunc antworten
  (sigo "problem" "claude-h")
  (sigo "problem" "gemini-p")
  (sigo "problem" "gpt41"))
```

**Wichtig für sigo-Prompts:** Den Prompt so formulieren dass die KI
*nur* den Lisp-Code zurückgibt ohne Erklärungen – z.B.:
`"Schreibe nur den Lisp-Code, keine Erklärungen: defun fib ..."`

---

## Build & Test

```bash
go build .           # kompilieren
go run . -t          # Testmodus (26 Tests)
go run .             # REPL starten
go run . skript.lisp # Datei ausführen
go test ./...        # Go-Unit-Tests
```

---

## Philosophie

GoLisp ist ein **U-Boot-Projekt** – es reift in Ruhe bevor es
der Welt gezeigt wird. Ziele:

- **Nexialistisch:** verbindet Go-Effizienz + Lisp-Eleganz + KI-Power
- **Selbsterweiternd:** GoLisp kann sich durch KI-Calls selbst vervollständigen
- **Ensemble-fähig:** mehrere KIs parallel → Synthese durch Claude
- **Centaur-Ansatz:** Mensch als Meta-Entscheider, KIs als Spezialisten

> "Code = Daten + KI = sich selbst erweiterndes System"
> – Gerhard & Claude, Februar 2026
