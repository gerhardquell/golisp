# GoLisp – offene Punkte / nächste Schritte

> Stand: 2026-06-15  
> Ausgangslage: Projektanalyse durch kimi abgeschlossen, Codebasis verstanden.

---

## Heute erreicht

- [x] CLAUDE.md und Projektbeschränkungen verstanden
- [x] Gesamte GoLisp-Codebasis analysiert
- [x] Build getestet: `go build .`, `go build ./cmd/golispd/`, `go build ./cmd/golisp-client/` → OK
- [x] Tests eingeschränkt lauffähig: `go test ./lib/...` → OK; `go test ./...` scheitert an `certs/`

---

## Priorisierte TODOs

### 1. eval.go aufgeteilt ✓ (2026-06-16)
`lib/eval.go` (1003 Zeilen) → 6 Dateien, alle <300 Zeilen. Reines Move,
keine Logikänderung. Sicherheitsnetz (36 Tests) blieb全程 grün, plus
End-to-End-Smoke-Tests (TCO 100k, quasiquote, closures, parfunc, macros).

- [x] `eval_core.go` (255) – Trampolin-Loop `Eval()`, evalArgs, apply,
      isTruthy, sliceToCell, cellToSlice. Tail-Forms (if/begin/let/let*/
      cond/case) bleiben INLINE im Loop – auslagern würde TCO zerstören.
- [x] `eval_specialforms.go` (227) – define/setq, defun, lambda, defmacro,
      set!/setq*, begin, mapcar, and/or/not, macroexpand, case, wrapBegin
- [x] `eval_lambda.go` (131) – applyLambda, makeLambda, bindArgs, IsMacro
- [x] `eval_quasiquote.go` (79) – evalQuasiquote, evalQQ, evalQQList, appendList
- [x] `eval_control.go` (297) – while, do, flet, labels, block, return-from,
      catch, eval, parfunc, lock, blockReturn
- [x] `eval_load.go` (101) – load, LoadString, Pfad-Auflösung (GOLISP_PATH)

### 2. stdlib zentralisiert ✓ (2026-06-16)
`golispd` lud in `lib/swank/server.go` eine eigene inline-stdlib (20/52
Funktionen) statt der eingebetteten `stdlib.lisp` → Drift. Server-Clients
bekamen keine `iota`/`flatten`/`gcd` etc.

Lösung: Option B – gemeinsame `LoadStdlib(env)` in `lib/stdlib.go`.
`//go:embed` im Server nicht direkt nutzbar (embed verbietet `..`-Pfade),
daher `stdlib.lisp` nach `lib/` verschoben und dort zentral eingebettet.

- [x] `stdlib.lisp` (root) → `lib/stdlib.lisp` (git mv)
- [x] `libs/stdlib.lisp` (totes Duplikat, untracked) entfernt
- [x] `lib/stdlib.go`: `//go:embed stdlib.lisp` + `LoadStdlib(env *Env) error`
- [x] `main.go`: embed+LoadString → `lib.LoadStdlib(env)`
- [x] `lib/swank/server.go`: `loadStdlib()`+inline-String → `lib.LoadStdlib(s.env)`
      (server.go 304→251 Zeilen)
- [x] Verifikation: CLI + Server liefern beide volle 52-Funktionen-stdlib
      (iota/flatten/gcd/length/cadr über Server-Client getestet)

### 3. Testinfrastruktur ausbauen (mittel)
Aktuell `lib/env_test.go` (27 Zeilen) + `lib/reader_test.go` (13 Tests).

- [x] Reader-Tests (2026-06-16): Atome, Zahlen, Strings+Escapes, Listen,
      nil/NIL, quote/quasiquote/unquote/splice, dispatch #', dotted pair,
      Kommentare, Whitespace, Fehlerfälle, 50-fache Verschachtelung
- [x] Eval-Grundlagen-Tests (2026-06-16): Arithmetik, Vergleiche, Booleans,
      if, begin, let/let*, cond, case, lambda, defun-Rekursion, Closures,
      quote, setq/setq*, quasiquote, eq/equal?, catch, Fehlerfälle.
      **TCO-Schutz:** 200.000-fache Tail-Rekursion (if+begin) grün.
- [x] Primitiven-Tests (2026-06-16): mod/abs, Typ-Prädikate, Listen-Edges,
      String-Funktionen (length/append/substring/upcase/downcase/->number/
      ->list/contains/replace/trim), fileio (write/append/read/exists?/delete
      mit TempDir), gensym, error, memstats. 13 Tests.
      **IST-Funde:** eq?=Pointer wie eq; file-write/append geben Pfad zurück
      (nicht t); atom? auf '()=t (leere Liste ist Atom-Typ).
- [x] Makro-Expansion-Tests (2026-06-16): defmacro+Aufruf, unevaluierte
      Args, macroexpand (incl. Nicht-Makro/Error), geschachtelte Makros,
      &rest, Arity-Fehler, IsMacro (Go). 12 Tests.
      **IST-Fund:** `setq` (=define=env.Set) im inneren let-Body shadowed
      die äußere Variable statt zu updaten – `set!` (env.Update) nötig
      für swap-Makros. Wichtig für Makro-Autoren.
- [x] `parfunc` / Channel-Tests (2026-06-16): parfunc (basic, order,
      error→nil, empty, timeout 1s, Rückgabetyp), buffered channels
      (FIFO, send-returns-value), lock-make/lock basic + Syntax-Fehler.
      12 Tests, nur deterministische Muster (kein blocking-send-ohne-receiver).
      **IST-Fund:** `(parfunc r)` ohne Expr setzt `r` NICHT im env
      (frühes `return MakeNil()` vor env.Set) – Mini-Bug.

**Reader-Test-Erkenntnisse (für eval.go-Split relevant):**
- `Cell.String()` rendert `NIL`-Cell als `"()"`, nil-Pointer als `"NIL"`.
  Spätere Eval-Asserts müssen das beachten.
- Backslash außerhalb eines Strings ist ein Symbol, kein Fehler.
- Dotted-pair-Reader verschluckt blind das Zeichen nach cdr (Todo #7) –
  als IST dokumentiert, kein Fix in den Tests.

**Eval-Test-Erkenntnisse (latente Bugs, NICHT eval.go-Split-relevant):**
- `(+ 1 "x")` = 1, kein Fehler: Arithmetik-Primitive greifen direkt auf
  `.Num` zu, Strings haben Num=0 → stille Typkoersion. Fix gehört in
  `primitives.go` (Todo #7-Nachfolger), nicht eval.go.
- `(- 5)` = 5: kein unäres Minus, `fnSub(1 Arg)` = `args[0]`.
- `(if)` = `()`: degenerierte if-Form ohne cond/else → Nil, kein Fehler.
- Sicherheitsnetz für eval.go-Split steht: 36 Tests gesamt (15+21).

### 4. `certs/`-Berechtigungsproblem gelöst ✓ (2026-06-16)
`certs/` enthielt verwaistes sigoREST-Cert-Paar (`server.crt`/`server.key`),
von GoLisp-Code und laufendem sigoREST ungenutzt (live-Certs liegen in
`/usr/local/slib/sigoREST/certs/`, anderer Fingerprint). Toter Ballast
inkl. privatem Key im Repo-Dir.

- [x] Prüfung: nicht GoLisp, nicht sigoREST-Prozess → entbehrlich
- [x] `certs/` gelöscht, `.gitignore`-Guard (`certs/`, `*.crt`, `*.key`, `*.pem`)
- [x] `go test ./...` läuft wieder vollständig durch

### 5. Code-Duplikation bereinigen (niedrig)
- [ ] `sliceToCell` / `CellToSlice` – Versionen in `eval.go` und `types_helpers.go` vereinheitlichen
- [ ] `isTruthy` / `IsTruthy` zusammenführen
- [ ] `countParens` / `countDepth` – gemeinsame Hilfsfunktion in `lib/`

### 6. sigoREST-Konfiguration verbessern (niedrig)
- [ ] Default-Modell (`ollama-gemma3-4b`) zentral als Konstante oder Env-Var konfigurierbar machen
- [ ] sigo-Host über Umgebungsvariable steuerbar (aktuell nur via `(sigo-host ...)` oder Parameter)

### 7. Kleinere Qualitätsverbesserungen (niedrig)
- [ ] Einrückung vereinheitlichen (Tabs vs. 2 Spaces)
- [ ] PostgreSQL-Connection nicht als `Cell{Type: LIST}` zurückgeben – eigener Typ oder Markierung
- [ ] `reader.go`: `)` nach dotted-pair explizit prüfen
- [ ] Mehr `nil`-Prüfungen in `eval*`-Helfern

---

## Offene Fragen für morgen

1. Soll zuerst `eval.go` aufgeteilt werden, oder lieber die Testabdeckung?
2. Soll `golispd` die gleiche `stdlib.lisp` wie die CLI laden, oder eine abgespeckte Server-Variante?
3. Was ist der Zweck von `certs/`? Kann es aus dem Repo entfernt oder verschoben werden?

---

## Nächster Sitzungsfokus (Vorschlag)

1. `eval.go` in sinnvolte Dateien aufteilen
2. Build und Tests (`go test ./lib/...`) weiterhin grün halten
3. Erste Tests für Reader oder Eval ergänzen
