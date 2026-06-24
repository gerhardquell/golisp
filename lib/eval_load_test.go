//**********************************************************************
//  lib/eval_load_test.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260624
//**********************************************************************

package lib

import (
  "os"
  "path/filepath"
  "testing"
)

func TestLoadStampsSrcFile(t *testing.T) {
  dir := t.TempDir()
  path := filepath.Join(dir, "src.lisp")
  if err := os.WriteFile(path, []byte("(defun h () 1)\n"), 0644); err != nil {
    t.Fatalf("write: %v", err)
  }
  env := BaseEnv()
  if _, err := LoadString(`(load "`+path+`")`, env); err != nil {
    t.Fatalf("load: %v", err)
  }
  loc, ok := LookupDefinition("h")
  if !ok {
    t.Fatalf("Definition h nicht registriert")
  }
  if loc.File != path {
    t.Fatalf("SrcFile = %q erwartet, got %q", path, loc.File)
  }
  if loc.Line != 1 {
    t.Fatalf("SrcLine = 1 erwartet, got %d", loc.Line)
  }
}

func TestDefunRegistersLocation(t *testing.T) {
  ClearDefinitions()
  env := BaseEnv()
  src := "(defun sq (x) (* x x))"
  form, err := Read(src)
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  form.SrcFile = "/test.lisp"
  form.SrcLine = 3
  if _, err := Eval(form, env); err != nil {
    t.Fatalf("eval: %v", err)
  }
  loc, ok := LookupDefinition("sq")
  if !ok {
    t.Fatalf("sq nicht registriert")
  }
  if loc.File != "/test.lisp" || loc.Line != 3 {
    t.Fatalf("got %+v", loc)
  }
}