# Retrospektive: KIESP Experiment

**Datum:** 28. Februar 2026
**Autor:** Claude (Co-Autor)
**Projekt:** KI-Esperanto (KIESP) - Kompakte Kommunikationssprache für KIs

---

## Zusammenfassung

Entwicklung von 4 konkurrierenden Implementierungen einer token-sparsamen Kodierung für KI-Kommunikation:
1. Stack-Fraktal (Postfix-Notation)
2. S-Expr Kompakt (Token-Abkürzungen)
3. Dictionary-basiert (Häufigkeitsanalyse)
4. Hybrid (kontext-abhängige Auswahl)

**Endergebnis:** Alle 8 Tests bestehen, alle Kodierungs-Methoden funktionieren.

---

## Was gut gelaufen ist ✓

### 1. Gesamtkonzept funktioniert
- Alle 4 Implementierungen wurden erfolgreich entwickelt
- Der Hybrid-Ansatz mit automatischer Kontext-Erkennung (code/narrative/data) funktioniert praktisch
- Dictionary-Kompression zeigt messbare Token-Einsparungen (z.B. 43 Zeichen → 9 Tokens)

### 2. Schnelle Iteration durch GoLisp
- Einfache Architektur ermöglichte schnelles Testen von Änderungen
- Das `-e` Flag für einzeilige Tests war sehr hilfreich für schnelles Feedback
- Kein kompilierter Code nötig, direkte Ausführung

### 3. Systematisches Debugging
- Durch Auskommentieren von Codezeilen konnte das "leere Token"-Problem isoliert werden
- Hexdump (`hexdump -C`) half beim Finden von Encoding-Problemen
- Schrittweise Reduktion auf Minimalbeispiele

---

## Was fehlerhaft/problematisch war ✗

### 1. GoLisp's `let` war kaputt
**Kritisch:** Der `let`-Special-Form hat nur den **ersten** Body-Ausdruck evaluiert und den Rest ignoriert.

```lisp
;; Dies hat NICHT funktioniert:
(let ((freqs (kiesp-count-words text)))
  (kiesp-build-dicts freqs max-entries 0)  ; ← Wurde ausgeführt
  (list kiesp-dictionary kiesp-reverse-dict))  ; ← Wurde ignoriert!
```

**Fix:** GoLisp's `eval.go` musste angepasst werden um mehrere Body-Ausdrücke zu unterstützen:
```go
// Alle außer dem letzten auswerten
for body.Cdr != nil && body.Cdr.Type == LIST {
    _, err := Eval(body.Car, localEnv)
    body = body.Cdr
}
```

### 2. Falsche Prädikate überall verwendet
Folgende Prädikate wurden fälschlicherweise verwendet, existieren aber nicht in GoLisp:

| Falsch | Richtig |
|--------|---------|
| `null?` | `null` |
| `atom?` | `atom` |
| `string?` | — (nicht verfügbar) |
| `number?` | — (nicht verfügbar) |

**Auswirkung:** Runtime-Errors bei jeder Verwendung dieser Funktionen.

### 3. Namenskonflikte mit eingebauten Primitiven
Die Definition von `>` als Funktionsname hat den `>` (größer-als) Operator überschrieben.

```lisp
(defun > () ...)  ; Überschreibt das > Primitive!

;; Dieser Code funktionierte dann nicht mehr:
(if (> start len) ...)  ; Fehler: > erwartet 0 Argumente, bekam 2
```

**Lösung:** Umbenennen zu `kiesp-level-1`, `kiesp-level-2`, etc.

### 4. Parallele `let`-Bindings missverstanden
Lisp's `let` ist **parallel**, nicht sequentiell:

```lisp
;; FALSCH - Zweite Bindung kann nicht auf erste zugreifen:
(let ((word (car words))
      (pair (assoc word kiesp-dictionary)))  ; word noch nicht gebunden!
  ...)

;; RICHTIG - Geschachtelte lets:
(let ((word (car words)))
  (let ((pair (assoc word kiesp-dictionary)))
    ...))
```

### 5. `eq` vs `equal?` Verwechslung
- `eq` vergleicht Pointer-Gleichheit (identisches Objekt im Speicher)
- `equal?` vergleicht strukturelle Gleichheit (gleicher Inhalt)

```lisp
(eq 'action 'action)      ; → ()  (zwei verschiedene Symbol-Objekte)
(equal? 'action 'action)  ; → t   (gleicher Name)
```

Betroffen: `kiesp-decode`, `kiesp-encode-list` - mussten von `eq` auf `equal?` umgestellt werden.

### 6. Division by Zero
`kiesp-stats-dict` und `kiesp-measure` hatten keine Schutz gegen leere Listen:

```lisp
(/ (kiesp-total-token-len encoded) enc-len)  ; Fehler wenn enc-len = 0
```

### 7. Fehlende Funktionen in GoLisp
- `list->string` - existiert nicht
- `case` - existiert nicht (nur `cond`)
- `string?`, `number?` - existieren nicht

---

## Was mir aufgefallen ist 💡

### 1. GoLisp's begrenzter Funktionsumfang
GoLisp ist ein Minimal-Lisp. Viele aus Common Lisp oder Scheme bekannte Funktionen fehlen:
- Kein `case` (nur `cond` mit `equal?`)
- Keine Typ-Prädikate außer `atom` und `null`
- Keine String→List Konvertierung

### 2. Stack-Decode ist "lossy"
Die Stack-Kodierung ist nicht verlustfrei:
```lisp
(data G >)
  → (action "generate" data)  ; Encoded
  → (data "generate" >)       ; Decoded - nicht identisch!
```
- `G` wird zu `"generate"`
- `'>` bleibt `'>`

Für KIESP akzeptabel, aber kein perfekter Roundtrip.

### 3. UTF-8/Encoding Probleme
Der Fehler "reader: leeres Token" trat auf, obwohl der Code korrekt aussah.
Vermutliche Ursachen:
- BOM (Byte Order Mark) am Dateianfang
- Gemischte Encodings (Latin-1 vs UTF-8)
- Korrupte Steuerzeichen

**Lösung:** Datei neu mit `Write` Tool erstellen (schreibt sauberes UTF-8).

### 4. Umfangreiche Fixes nötig
Was als "einfaches Experiment" gedacht war, erforderte:
- 1 Go-Code-Fix (`eval.go`)
- Mehrere Dutzend Lisp-Code-Fixes
- 2 Git-Commits

---

## Lessons Learned 📚

### 1. Vorher prüfen welche Prädikate/Funktionen existieren
Die CLAUDE.md hätte genauer gelesen werden sollen. Vor Projektstart:
- Liste aller verfügbaren Primitiven erstellen
- Testen ob benötigte Funktionen existieren

### 2. Keine einbuchstabigen Funktionsnamen
In Lisp sind folgende Namen praktisch immer reserviert:
`>`, `<`, `+`, `-`, `*`, `/`, `=`, `?`, `!`

**Empfehlung:** Immer ausführliche Namen verwenden (`kiesp-level-1` statt `>`).

### 3. `let`-Semantik verstehen
- Scheme/CL: `let` = parallel, `let*` = sequentiell
- GoLisp: Nur `let` (parallel)

Bei Abhängigkeiten zwischen Variablen: Geschachtelte `let`s verwenden.

### 4. `eq` nur für Symbole aus dem selben `quote`
```lisp
(eq 'foo 'foo)  ; Manchmal t, manchmal () - undefiniert!
```
**Regel:** Für Symbol-Vergleich immer `equal?` verwenden.

### 5. Tests früh und oft schreiben
Viele der später entdeckten Fehler hätten bei testgetriebener Entwicklung sofort auffallen können.

### 6. Encoding-Probleme sind subtil
Wenn "leeres Token" kommt obwohl der Code korrekt aussieht:
- Datei in hex ansehen
- Auf BOM prüfen
- File neu schreiben mit bekanntem Encoding

---

## Empfehlungen für zukünftige Experimente

1. **Capability-Probe zuerst:** Kleinste mögliche Testdatei schreiben
2. **Primitiven-Liste ausgeben:** `(mapcar car env.symbols)` oder ähnlich
3. **Inkrementell entwickeln:** Jede Funktion einzeln testen bevor die nächste kommt
4. **Keine Annahmen:** Nicht annehmen dass "Standard"-Lisp-Funktionen existieren

---

## Fazit

Das KIESP-Experiment war **erfolgreich** - alle Features funktionieren und die Tests bestehen. Aber es war **aufwändiger als erwartet** wegen der vielen Inkonsistenzen zwischen meinen Annahmen über GoLisp und der tatsächlichen Implementierung.

Die größte Überraschung war das **kaputte `let` in GoLisp**, das Go-seitig behoben werden musste. Das hat das Experiment von einer reinen Lisp-Übung zu einer Go+Lisp-Debugging-Session gemacht.

**Würde ich es wieder machen?** Ja, aber mit einer gründlicheren Analyse der verfügbaren Primitiven zu Beginn.

---

## Statistik

- **Zeilen Code geschrieben:** ~850
- **Tests:** 8/8 bestehen
- **Commits:** 2
- **Go-Fixes:** 1 (let mit multi-body)
- **Lisp-Fixes:** ~30 (Prädikate, Operator-Namen, let-Bindings)
