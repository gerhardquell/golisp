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
  "strings"
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

func TestSwankPrintReturnValue(t *testing.T) {
  env := lib.BaseEnv()
  var sent *lib.Cell
  send := func(c *lib.Cell) error {
    sent = c
    return nil
  }
  RegisterSwankEnv(env, send)

  // (swank-print "hello")
  cell, err := lib.Read("(swank-print \"hello\")")
  if err != nil {
    t.Fatalf("read failed: %v", err)
  }
  result, err := lib.Eval(cell, env)
  if err != nil {
    t.Fatalf("eval failed: %v", err)
  }
  // Event darf kein :repl-result tragen
  if sent == nil {
    t.Fatal("send callback was not invoked")
  }
  if sent.String() != `(:write-string "\"hello\"")` {
    t.Fatalf("unexpected event: %s", sent.String())
  }
  // Rückgabewert muss das letzte Argument sein
  if result == nil || result.String() != "\"hello\"" {
    t.Fatalf("expected return value \"hello\", got: %v", result)
  }
}

func TestSwankFindDefinition(t *testing.T) {
  env := lib.BaseEnv()
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  lib.RegisterDefinition("found", "/x.lisp", 9)

  // Teste via env.Get() + direktem Fn-Aufruf (kein callPrimitive-Helper vorhanden)
  cell, err := env.Get("swank--find-definition")
  if err != nil {
    t.Fatalf("env.Get failed: %v", err)
  }
  if cell.Type != lib.FUNC {
    t.Fatalf("expected FUNC, got %v", cell.Type)
  }

  result, err := cell.Fn([]*lib.Cell{lib.MakeStr("found")})
  if err != nil {
    t.Fatalf("call failed: %v", err)
  }
  s := result.String()
  if !strings.Contains(s, "/x.lisp") || !strings.Contains(s, "9") {
    t.Fatalf("expected (/x.lisp . 9), got %s", s)
  }

  lib.ClearDefinitions()
  result2, err := cell.Fn([]*lib.Cell{lib.MakeStr("missing")})
  if err != nil {
    t.Fatalf("call failed: %v", err)
  }
  if result2.Type != lib.NIL {
    t.Fatalf("expected NIL für missing, got %v", result2)
  }
}
