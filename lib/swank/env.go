//**********************************************************************
//  lib/swank/env.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260618
//**********************************************************************
// Per-connection SWANK primitives: send-event, print, println,
// value-string.
//**********************************************************************

package swank

import (
  "fmt"
  "strings"

  "golisp/lib"
)

// RegisterSwankEnv registers connection-bound SWANK primitives.
// send writes an event Cell to Emacs.
func RegisterSwankEnv(env *lib.Env, send func(*lib.Cell) error) {
  env.Set("swank-send-event", makeFn(func(args []*lib.Cell) (*lib.Cell, error) {
    if len(args) < 1 {
      return nil, fmt.Errorf("swank-send-event: 1 Argument nötig")
    }
    if err := send(args[0]); err != nil {
      return nil, fmt.Errorf("swank-send-event: %w", err)
    }
    return lib.MakeNil(), nil
  }))

  env.Set("swank-print", makeFn(func(args []*lib.Cell) (*lib.Cell, error) {
    return swankPrint(args, send, false)
  }))

  env.Set("swank-println", makeFn(func(args []*lib.Cell) (*lib.Cell, error) {
    return swankPrint(args, send, true)
  }))

  env.Set("swank--value-string", makeFn(func(args []*lib.Cell) (*lib.Cell, error) {
    if len(args) < 1 {
      return nil, fmt.Errorf("swank--value-string: 1 Argument nötig")
    }
    return lib.MakeStr(args[0].String()), nil
  }))
}

func makeFn(f func([]*lib.Cell) (*lib.Cell, error)) *lib.Cell {
  return &lib.Cell{Type: lib.FUNC, Fn: f}
}

func swankPrint(args []*lib.Cell, send func(*lib.Cell) error, newline bool) (*lib.Cell, error) {
  var b strings.Builder
  for i, a := range args {
    if i > 0 {
      b.WriteString(" ")
    }
    b.WriteString(a.String())
  }
  if newline {
    b.WriteString("\n")
  }
  event := lib.Cons(
    lib.MakeAtom(":write-string"),
    lib.Cons(
      lib.MakeStr(b.String()),
      lib.Cons(lib.MakeAtom(":repl-result"), lib.MakeNil()),
    ),
  )
  if err := send(event); err != nil {
    return nil, fmt.Errorf("swank-print: %w", err)
  }
  return lib.MakeNil(), nil
}
