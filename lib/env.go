//**********************************************************************
//  lib/env.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260223
//**********************************************************************

package lib

import "fmt"

// Env ist eine verkettete Umgebung: lokaler Scope -> aeusserer Scope.
// Optimierung: der erste gebundene Name wird inline gespeichert, da die
// meisten Lambda-Calls nur einen Parameter haben (z.B. fib n). Erst ab
// dem zweiten Symbol wird die Map alloziert.
type Env struct {
  vars       map[string]*Cell
  parent     *Env
  singleName string
  singleVal  *Cell
}

func NewEnv(parent *Env) *Env {
  return &Env{parent: parent}
}

// Get sucht einen Namen – erst lokal, dann im aeusseren Scope
func (e *Env) Get(name string) (*Cell, error) {
  if e.singleName == name { return e.singleVal, nil }
  if val, ok := e.vars[name]; ok { return val, nil }
  if e.parent != nil { return e.parent.Get(name) }
  return nil, fmt.Errorf("env: unbekanntes Symbol '%s'", name)
}

// Set legt einen Wert im aktuellen Scope ab
func (e *Env) Set(name string, val *Cell) {
  if e.singleName == "" {
    e.singleName = name
    e.singleVal = val
    return
  }
  if e.singleName == name {
    e.singleVal = val
    return
  }
  if e.vars == nil {
    e.vars = make(map[string]*Cell)
  }
  e.vars[name] = val
}

// Root liefert die aeusserste Umgebung (Globalenv). Common-Lisp-Semantik
// fuer (eval form): Auswertung im globalen Environment, unabhaengig vom
// dynamischen Lambda-Scope. Ohne dies wuerde (defun ...) aus einem
// REPL-Eval heraus lokal im Child-Env definiert und ginge verloren.
func (e *Env) Root() *Env {
  cur := e
  for cur.parent != nil {
    cur = cur.parent
  }
  return cur
}

// Symbols sammelt alle bekannten Namen (inkl. aeussere Scopes, ohne Duplikate)
func (e *Env) Symbols() []string {
  seen := make(map[string]bool)
  var result []string
  for cur := e; cur != nil; cur = cur.parent {
    if cur.singleName != "" && !seen[cur.singleName] {
      seen[cur.singleName] = true
      result = append(result, cur.singleName)
    }
    for name := range cur.vars {
      if !seen[name] {
        seen[name] = true
        result = append(result, name)
      }
    }
  }
  return result
}

// Update aendert einen bestehenden Wert (fuer set!)
func (e *Env) Update(name string, val *Cell) error {
  if e.singleName == name {
    e.singleVal = val
    return nil
  }
  if _, ok := e.vars[name]; ok {
    e.vars[name] = val
    return nil
  }
  if e.parent != nil { return e.parent.Update(name, val) }
  return fmt.Errorf("env: set! – Symbol '%s' nicht gefunden", name)
}
