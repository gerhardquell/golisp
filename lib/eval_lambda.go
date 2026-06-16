//**********************************************************************
//  lib/eval_lambda.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260616 (aufgespalten aus eval.go)
//**********************************************************************
// Lambda/Closure-Logik: makeLambda, applyLambda, bindArgs, IsMacro.
// Lambda-Struktur: Cell{Type:LIST, Car:params, Cdr:body, Env:closureEnv}
// (Makros identisch, aber Type:MACRO.)
//**********************************************************************

package lib

import "fmt"

// makeLambda baut eine Closure-Cell (Type:LIST, Env=Closure).
func makeLambda(params, body *Cell, env *Env) *Cell {
  return &Cell{Type: LIST, Car: params, Cdr: body, Env: env}
}

// applyLambda wendet eine Lambda/Closure auf Argumente an.
// Wird auch für Makro-Expansion genutzt (siehe Eval + evalMacroexpand).
func applyLambda(lambda *Cell, args []*Cell) (*Cell, error) {
  closureEnv := lambda.Env.(*Env)
  localEnv   := NewEnv(closureEnv)
  if err := bindArgs(lambda.Car, args, closureEnv, localEnv); err != nil {
    return nil, err
  }
  return Eval(lambda.Cdr, localEnv)
}

// IsMacro prüft ob eine Cell ein Makro ist (exportiert für macroexpand).
func IsMacro(c *Cell) bool {
  return c != nil && c.Type == MACRO
}

// bindArgs: Lambda-Parameter binden – unterstützt regulär, dotted-rest,
// &optional, &key, &rest (CL-Stil Lambda-Listen).
func bindArgs(params *Cell, args []*Cell, closureEnv *Env, localEnv *Env) error {
  section := 0  // 0=regulär, 1=&optional, 2=&key
  argIdx  := 0
  hasKey  := false  // &key verwendet → kein excess check

  for p := params; p != nil; {
    if p.Type == NIL { break }
    if p.Type == ATOM {
      // Dotted rest-Parameter: (lambda (a b . rest) ...)
      localEnv.Set(p.Val, SliceToCell(args[argIdx:]))
      return nil
    }
    if p.Type != LIST { break }

    param := p.Car
    p = p.Cdr

    if param.Type == ATOM {
      switch param.Val {
      case "&optional": section = 1; continue
      case "&key":      section = 2; hasKey = true; continue
      case "&rest":
        if p == nil || p.Type != LIST || p.Car == nil {
          return fmt.Errorf("lambda: &rest braucht Parameter-Namen")
        }
        localEnv.Set(p.Car.Val, SliceToCell(args[argIdx:]))
        return nil
      }
    }

    switch section {
    case 0:  // reguläre Parameter
      if param.Type != ATOM {
        return fmt.Errorf("lambda: Parameter muss Atom sein")
      }
      if argIdx >= len(args) {
        return fmt.Errorf("lambda: zu wenig Argumente (brauche '%s')", param.Val)
      }
      localEnv.Set(param.Val, args[argIdx])
      argIdx++

    case 1:  // &optional
      var name string
      var def  *Cell
      if param.Type == LIST {
        name = param.Car.Val
        if param.Cdr != nil && param.Cdr.Type == LIST { def = param.Cdr.Car }
      } else {
        name = param.Val
      }
      if argIdx < len(args) {
        localEnv.Set(name, args[argIdx]); argIdx++
      } else if def != nil {
        val, err := Eval(def, closureEnv)
        if err != nil { return err }
        localEnv.Set(name, val)
      } else {
        localEnv.Set(name, MakeNil())
      }

    case 2:  // &key
      var name string
      var def  *Cell
      if param.Type == LIST {
        name = param.Car.Val
        if param.Cdr != nil && param.Cdr.Type == LIST { def = param.Cdr.Car }
      } else {
        name = param.Val
      }
      keyword := ":" + name
      found := false
      for ki := argIdx; ki < len(args); ki++ {
        if args[ki].Type == ATOM && args[ki].Val == keyword && ki+1 < len(args) {
          localEnv.Set(name, args[ki+1]); found = true; break
        }
      }
      if !found {
        if def != nil {
          val, err := Eval(def, closureEnv)
          if err != nil { return err }
          localEnv.Set(name, val)
        } else {
          localEnv.Set(name, MakeNil())
        }
      }
    }
  }
  if !hasKey && argIdx < len(args) {
    return fmt.Errorf("lambda: zu viele Argumente (%d überzählig)", len(args)-argIdx)
  }
  return nil
}
