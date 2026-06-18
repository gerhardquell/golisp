//**********************************************************************
//  lib/swank/env_test.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260618
//**********************************************************************
// Tests für per-connection SWANK primitives.
//**********************************************************************

package swank

import (
  "testing"

  "golisp/lib"
)

func TestRegisterSwankEnv(t *testing.T) {
  env := lib.BaseEnv()
  var sent *lib.Cell
  send := func(c *lib.Cell) error {
    sent = c
    return nil
  }
  RegisterSwankEnv(env, send)

  // (swank-send-event '(:write-string "hi" :repl-result))
  cell, err := lib.Read("(swank-send-event '(:write-string \"hi\" :repl-result))")
  if err != nil {
    t.Fatalf("read failed: %v", err)
  }
  _, err = lib.Eval(cell, env)
  if err != nil {
    t.Fatalf("eval failed: %v", err)
  }
  if sent == nil {
    t.Fatal("send callback was not invoked")
  }
  if sent.String() != "(:write-string \"hi\" :repl-result)" {
    t.Fatalf("unexpected event: %s", sent.String())
  }
}
