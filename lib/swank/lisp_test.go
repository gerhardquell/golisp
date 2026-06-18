//**********************************************************************
//  lib/swank/lisp_test.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260618
//**********************************************************************
// Smoke test für eingebettete SWANK Lisp-Handler.
//**********************************************************************

package swank

import (
  "strings"
  "testing"

  "golisp/lib"
)

func TestSwankLisp(t *testing.T) {
  env := lib.BaseEnv()
  lib.LoadStdlib(env)
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  if err := LoadSwankLisp(env); err != nil {
    t.Fatalf("LoadSwankLisp: %v", err)
  }
  cell, err := lib.Read("(:emacs-rex (swank:connection-info) nil t 1)")
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  result, err := HandleMessage(env, cell)
  if err != nil {
    t.Fatalf("HandleMessage: %v", err)
  }
  if result == nil || !strings.Contains(result.String(), ":return") {
    t.Fatalf("unexpected result: %v", result)
  }
}
