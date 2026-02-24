//**********************************************************************
//  lib/readline.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260224
//**********************************************************************
// Readline mit github.com/chzyer/readline:
//   - History (Pfeiltasten)
//   - Multiline: offene Klammern → Fortsetzung mit ..
//   - Ctrl+C: Eingabe abbrechen
//   - Ctrl+D: Beenden
//**********************************************************************

package lib

import (
  "fmt"
  "io"
  "strings"

  "github.com/chzyer/readline"
)

type Readline struct {
  rl      *readline.Instance
  history []string
}

func NewReadline(prompt string) *Readline {
  rl, err := readline.NewEx(&readline.Config{
    Prompt:          prompt,
    HistoryLimit:    200,
    InterruptPrompt: "^C",
    EOFPrompt:       "exit",
  })
  if err != nil {
    // Fallback falls Terminal nicht unterstützt wird
    fmt.Println("WARN: readline nicht verfügbar, einfacher Modus")
    return &Readline{}
  }
  return &Readline{rl: rl}
}

// Read liest einen vollständigen Lisp-Ausdruck – auch über mehrere Zeilen
func (r *Readline) Read() (string, error) {
  if r.rl == nil { return r.readSimple() }

  var lines []string
  prompt := r.rl.Config.Prompt

  for {
    r.rl.SetPrompt(prompt)
    line, err := r.rl.Readline()
    if err == readline.ErrInterrupt {
      // Ctrl+C → aktuelle Eingabe verwerfen
      lines = nil
      prompt = r.rl.Config.Prompt
      fmt.Println()
      continue
    }
    if err == io.EOF { return "", fmt.Errorf("EOF") }
    if err != nil   { return "", err }

    lines = append(lines, line)
    combined := strings.Join(lines, " ")

    // Klammern zählen: ist der Ausdruck vollständig?
    depth := countDepth(combined)
    if depth == 0 && strings.TrimSpace(combined) != "" {
      return strings.TrimSpace(combined), nil
    }
    if depth < 0 {
      // Zu viele schließende Klammern
      fmt.Println("ERR: unerwartete ')'")
      lines = nil
      prompt = r.rl.Config.Prompt
      continue
    }
    // Ausdruck noch offen → Fortsetzung
    prompt = fmt.Sprintf("..%s", strings.Repeat("  ", depth))
  }
}

// countDepth zählt offene Klammern (ignoriert Strings und Kommentare)
func countDepth(s string) int {
  depth  := 0
  inStr  := false
  escape := false

  for _, ch := range s {
    if escape  { escape = false; continue }
    if ch == '\\' && inStr { escape = true; continue }
    if ch == '"' { inStr = !inStr; continue }
    if inStr    { continue }
    if ch == ';' { break }  // Kommentar bis Zeilenende
    if ch == '(' { depth++ }
    if ch == ')' { depth-- }
  }
  return depth
}

// readSimple: Fallback ohne readline
func (r *Readline) readSimple() (string, error) {
  fmt.Print("golisp> ")
  var line string
  _, err := fmt.Scanln(&line)
  return line, err
}

// Close gibt readline-Ressourcen frei
func (r *Readline) Close() {
  if r.rl != nil { r.rl.Close() }
}
