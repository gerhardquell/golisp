//**********************************************************************
//  lib/env.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260223
//**********************************************************************

package lib

import "fmt"

// Env ist eine verkettete Umgebung: lokaler Scope → äußerer Scope
type Env struct {
  vars   map[string]*Cell
  parent *Env
}

func NewEnv(parent *Env) *Env {
  return &Env{vars: make(map[string]*Cell), parent: parent}
}

// Get sucht einen Namen – erst lokal, dann im äußeren Scope
func (e *Env) Get(name string) (*Cell, error) {
  if val, ok := e.vars[name]; ok { return val, nil }
  if e.parent != nil { return e.parent.Get(name) }
  return nil, fmt.Errorf("env: unbekanntes Symbol '%s'", name)
}

// Set legt einen Wert im aktuellen Scope ab
func (e *Env) Set(name string, val *Cell) {
  e.vars[name] = val
}

// Update ändert einen bestehenden Wert (für set!)
func (e *Env) Update(name string, val *Cell) error {
  if _, ok := e.vars[name]; ok {
    e.vars[name] = val
    return nil
  }
  if e.parent != nil { return e.parent.Update(name, val) }
  return fmt.Errorf("env: set! – Symbol '%s' nicht gefunden", name)
}
