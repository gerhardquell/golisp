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
  "fmt"
  "os"
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

func TestSwankOperatorArglistBuiltIn(t *testing.T) {
  env := lib.BaseEnv()
  lib.LoadStdlib(env)
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  if err := LoadSwankLisp(env); err != nil {
    t.Fatalf("LoadSwankLisp: %v", err)
  }
  cell, err := lib.Read("(:emacs-rex (swank:operator-arglist \"car\" \"USER\") nil t 1)")
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  result, err := HandleMessage(env, cell)
  if err != nil {
    t.Fatalf("HandleMessage: %v", err)
  }
  s := result.String()
  if !strings.Contains(s, "(car list)") {
    t.Fatalf("expected built-in arglist (car list), got: %s", s)
  }
}

func TestSwankAutodocBuiltIn(t *testing.T) {
  env := lib.BaseEnv()
  lib.LoadStdlib(env)
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  if err := LoadSwankLisp(env); err != nil {
    t.Fatalf("LoadSwankLisp: %v", err)
  }
  cell, err := lib.Read("(:emacs-rex (swank:autodoc (quote (car x)) \"USER\") nil t 1)")
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  result, err := HandleMessage(env, cell)
  if err != nil {
    t.Fatalf("HandleMessage: %v", err)
  }
  s := result.String()
  if !strings.Contains(s, "(car list)") {
    t.Fatalf("expected autodoc (car list), got: %s", s)
  }
  if strings.Contains(s, ":not-available") {
    t.Fatalf("autodoc should be available for built-in car, got: %s", s)
  }
}

func TestSwankOperatorArglistLambda(t *testing.T) {
  env := lib.BaseEnv()
  lib.LoadStdlib(env)
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  if err := LoadSwankLisp(env); err != nil {
    t.Fatalf("LoadSwankLisp: %v", err)
  }
  defCell, err := lib.Read("(defun mytestfn (a b) (+ a b))")
  if err != nil {
    t.Fatalf("read defun: %v", err)
  }
  if _, err := lib.Eval(defCell, env); err != nil {
    t.Fatalf("eval defun: %v", err)
  }
  cell, err := lib.Read("(:emacs-rex (swank:operator-arglist \"mytestfn\" \"USER\") nil t 1)")
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  result, err := HandleMessage(env, cell)
  if err != nil {
    t.Fatalf("HandleMessage: %v", err)
  }
  s := result.String()
  if !strings.Contains(s, "(mytestfn a b)") {
    t.Fatalf("expected lambda arglist (mytestfn a b), got: %s", s)
  }
  if strings.Contains(s, "(mytestfn &rest") {
    t.Fatalf("lambda should not fall back to built-in registry, got: %s", s)
  }
}

func TestSwankDescribeSymbolBuiltIn(t *testing.T) {
  env := lib.BaseEnv()
  lib.LoadStdlib(env)
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  if err := LoadSwankLisp(env); err != nil {
    t.Fatalf("LoadSwankLisp: %v", err)
  }
  cell, err := lib.Read("(:emacs-rex (swank:describe-symbol \"car\" \"USER\") nil t 1)")
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  result, err := HandleMessage(env, cell)
  if err != nil {
    t.Fatalf("HandleMessage: %v", err)
  }
  s := result.String()
  if !strings.Contains(s, ":title \"car\"") {
    t.Fatalf("expected title car, got: %s", s)
  }
  if !strings.Contains(s, "function") {
    t.Fatalf("expected type function, got: %s", s)
  }
  if !strings.Contains(s, "erste Element") {
    t.Fatalf("expected static description, got: %s", s)
  }
}

func TestSwankDescribeSymbolLambda(t *testing.T) {
  env := lib.BaseEnv()
  lib.LoadStdlib(env)
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  if err := LoadSwankLisp(env); err != nil {
    t.Fatalf("LoadSwankLisp: %v", err)
  }
  defCell, err := lib.Read("(defun mydescfn (x) (* x x))")
  if err != nil {
    t.Fatalf("read defun: %v", err)
  }
  if _, err := lib.Eval(defCell, env); err != nil {
    t.Fatalf("eval defun: %v", err)
  }
  cell, err := lib.Read("(:emacs-rex (swank:describe-symbol \"mydescfn\" \"USER\") nil t 1)")
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  result, err := HandleMessage(env, cell)
  if err != nil {
    t.Fatalf("HandleMessage: %v", err)
  }
  s := result.String()
  if !strings.Contains(s, ":title \"mydescfn\"") {
    t.Fatalf("expected title mydescfn, got: %s", s)
  }
  if !strings.Contains(s, "lambda") {
    t.Fatalf("expected type lambda, got: %s", s)
  }
  if !strings.Contains(s, "(mydescfn x)") {
    t.Fatalf("expected arglist (mydescfn x), got: %s", s)
  }
}

func TestSwankDescribeSymbolUnbound(t *testing.T) {
  env := lib.BaseEnv()
  lib.LoadStdlib(env)
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  if err := LoadSwankLisp(env); err != nil {
    t.Fatalf("LoadSwankLisp: %v", err)
  }
  cell, err := lib.Read("(:emacs-rex (swank:describe-symbol \"definitely-not-bound-symbol\" \"USER\") nil t 1)")
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  result, err := HandleMessage(env, cell)
  if err != nil {
    t.Fatalf("HandleMessage: %v", err)
  }
  s := result.String()
  if !strings.Contains(s, ":title \"definitely-not-bound-symbol\"") {
    t.Fatalf("expected title, got: %s", s)
  }
  if !strings.Contains(s, "unbound") {
    t.Fatalf("expected unbound type, got: %s", s)
  }
}

func TestSwankCompileString(t *testing.T) {
  env := lib.BaseEnv()
  lib.LoadStdlib(env)
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  if err := LoadSwankLisp(env); err != nil {
    t.Fatalf("LoadSwankLisp: %v", err)
  }
  cell, err := lib.Read("(:emacs-rex (swank:compile-string-for-emacs \"(defun compile-string-test () 123)\") nil t 1)")
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  result, err := HandleMessage(env, cell)
  if err != nil {
    t.Fatalf("HandleMessage: %v", err)
  }
  s := result.String()
  if !strings.Contains(s, ":ok t") {
    t.Fatalf("expected :ok t, got: %s", s)
  }
  checkCell, err := lib.Read("(compile-string-test)")
  if err != nil {
    t.Fatalf("read check: %v", err)
  }
  val, err := lib.Eval(checkCell, env)
  if err != nil {
    t.Fatalf("eval check: %v", err)
  }
  if val.String() != "123" {
    t.Fatalf("expected 123, got: %s", val.String())
  }
}

func TestSwankCompileFile(t *testing.T) {
  env := lib.BaseEnv()
  lib.LoadStdlib(env)
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  if err := LoadSwankLisp(env); err != nil {
    t.Fatalf("LoadSwankLisp: %v", err)
  }
  if err := os.MkdirAll("./tmp", 0755); err != nil {
    t.Fatalf("mkdir tmp: %v", err)
  }
  tmpFile := "./tmp/compile-file-test.lisp"
  if err := os.WriteFile(tmpFile, []byte("(defun compile-file-test () 456)"), 0644); err != nil {
    t.Fatalf("write tmp file: %v", err)
  }
  defer os.Remove(tmpFile)

  req := fmt.Sprintf("(:emacs-rex (swank:compile-file-for-emacs %q) nil t 1)", tmpFile)
  cell, err := lib.Read(req)
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  result, err := HandleMessage(env, cell)
  if err != nil {
    t.Fatalf("HandleMessage: %v", err)
  }
  s := result.String()
  if !strings.Contains(s, ":ok") || strings.Contains(s, ":abort") {
    t.Fatalf("expected ok result, got: %s", s)
  }
  checkCell, err := lib.Read("(compile-file-test)")
  if err != nil {
    t.Fatalf("read check: %v", err)
  }
  val, err := lib.Eval(checkCell, env)
  if err != nil {
    t.Fatalf("eval check: %v", err)
  }
  if val.String() != "456" {
    t.Fatalf("expected 456, got: %s", val.String())
  }
}

func TestSwankMacroexpandAll(t *testing.T) {
  env := lib.BaseEnv()
  lib.LoadStdlib(env)
  RegisterSwankEnv(env, func(c *lib.Cell) error { return nil })
  if err := LoadSwankLisp(env); err != nil {
    t.Fatalf("LoadSwankLisp: %v", err)
  }
  cell, err := lib.Read("(:emacs-rex (swank:swank-macroexpand-all \"(list (when t 1))\") nil t 1)")
  if err != nil {
    t.Fatalf("read: %v", err)
  }
  result, err := HandleMessage(env, cell)
  if err != nil {
    t.Fatalf("HandleMessage: %v", err)
  }
  s := result.String()
  if !strings.Contains(s, "(list (if t (begin 1) ()))") {
    t.Fatalf("expected recursive expansion, got: %s", s)
  }
  if strings.Contains(s, "when") {
    t.Fatalf("macroexpand-all should not leave when unexpanded, got: %s", s)
  }
}
