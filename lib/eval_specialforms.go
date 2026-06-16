//**********************************************************************
//  lib/eval_specialforms.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260616 (aufgespalten aus eval.go)
//**********************************************************************
// Spezialformen (nicht-tail): define/setq, defun, lambda, defmacro,
// set!/setq*, begin, mapcar, load (+ Pfad-Auflösung), LoadString,
// and/or/not, macroexpand, case (Tail-Hilfsfunktion, gibt Tripel zurück).
//**********************************************************************

package lib

import (
  "fmt"
)

func evalDefine(args *Cell, env *Env) (*Cell, error) {
  name := args.Car.Val
  val, err := Eval(args.Cdr.Car, env)
  if err != nil { return nil, err }
  env.Set(name, val)
  return MakeAtom(name), nil
}

// macroexpand: (macroexpand form) → expandiert Makros einmal, gibt Ergebnis zurück
func evalMacroexpand(args *Cell, env *Env) (*Cell, error) {
  if args == nil || args.Type != LIST || args.Car == nil {
    return nil, fmt.Errorf("macroexpand: 1 Argument nötig")
  }
  form, err := Eval(args.Car, env)
  if err != nil { return nil, err }

  // Wenn es keine Liste ist, geben wir sie unverändert zurück
  if form == nil || form.Type != LIST || form.Car == nil {
    return form, nil
  }

  // Prüfe ob das erste Element ein Makro ist
  fn, err := Eval(form.Car, env)
  if err != nil { return nil, err }

  // Wenn es ein Makro ist, expandieren wir es
  if fn.Type == MACRO {
    return applyLambda(fn, cellToSlice(form.Cdr))
  }

  // Kein Makro → Form unverändert zurückgeben
  return form, nil
}

// wrapBegin: mehrere Body-Ausdrücke → (begin expr1 expr2 ...)
// Einzelner Ausdruck → direkt zurückgeben (kein unnötiger begin-Wrapper)
func wrapBegin(exprs *Cell) *Cell {
  if exprs == nil || exprs.Type != LIST {
    return MakeNil()
  }
  if exprs.Cdr == nil || exprs.Cdr.Type != LIST {
    return exprs.Car  // nur ein Ausdruck → direkt
  }
  return Cons(MakeAtom("begin"), exprs)  // mehrere → (begin ...)
}

func evalDefun(args *Cell, env *Env) (*Cell, error) {
  name := args.Car.Val
  lam  := makeLambda(args.Cdr.Car, wrapBegin(args.Cdr.Cdr), env)
  env.Set(name, lam)
  return MakeAtom(name), nil
}

func evalLambda(args *Cell, env *Env) (*Cell, error) {
  return makeLambda(args.Car, wrapBegin(args.Cdr), env), nil
}

func evalBegin(args *Cell, env *Env) (*Cell, error) {
  var result *Cell
  var err error
  for args != nil && args.Type == LIST {
    result, err = Eval(args.Car, env)
    if err != nil { return nil, err }
    args = args.Cdr
  }
  return result, nil
}

func evalSet(args *Cell, env *Env) (*Cell, error) {
  val, err := Eval(args.Cdr.Car, env)
  if err != nil { return nil, err }
  return MakeAtom(args.Car.Val), env.Update(args.Car.Val, val)
}

// setq*: (setq* var1 val1 var2 val2 ...) → sequentielles Setzen
func evalSetQStar(args *Cell, env *Env) (*Cell, error) {
  if args == nil || args.Type != LIST {
    return nil, fmt.Errorf("setq*: Syntax: (setq* var1 val1 var2 val2 ...)")
  }
  var lastName string
  for a := args; a != nil && a.Type == LIST; a = a.Cdr.Cdr {
    if a.Car == nil || a.Car.Type != ATOM {
      return nil, fmt.Errorf("setq*: Variable muss ein Symbol sein")
    }
    name := a.Car.Val
    lastName = name
    if a.Cdr == nil || a.Cdr.Type != LIST {
      return nil, fmt.Errorf("setq*: Wert für '%s' fehlt", name)
    }
    val, err := Eval(a.Cdr.Car, env)
    if err != nil { return nil, err }
    // Update existierende Variable oder neu definieren
    if _, getErr := env.Get(name); getErr == nil {
      env.Update(name, val)  // Existiert → updaten
    } else {
      env.Set(name, val)     // Neu → definieren
    }
  }
  return MakeAtom(lastName), nil
}

// mapcar: (mapcar fn liste) → wendet fn auf jedes Element an
func evalMapcar(args *Cell, env *Env) (*Cell, error) {
  fn, err := Eval(args.Car, env)
  if err != nil { return nil, err }

  lst, err := Eval(args.Cdr.Car, env)
  if err != nil { return nil, err }

  var results []*Cell
  for lst != nil && lst.Type == LIST {
    res, err := apply(fn, []*Cell{lst.Car})
    if err != nil { return nil, err }
    results = append(results, res)
    lst = lst.Cdr
  }

  // Ergebnisliste aufbauen
  result := MakeNil()
  for i := len(results) - 1; i >= 0; i-- {
    result = Cons(results[i], result)
  }
  return result, nil
}

// and: (and a b c ...) → gibt ersten falschen Wert zurück, sonst letzten
func evalAnd(args *Cell, env *Env) (*Cell, error) {
  result := &Cell{Type: ATOM, Val: "t"}
  for args != nil && args.Type == LIST {
    val, err := Eval(args.Car, env)
    if err != nil { return nil, err }
    if !isTruthy(val) { return MakeNil(), nil }  // Kurzschluss!
    result = val
    args = args.Cdr
  }
  return result, nil
}

// or: (or a b c ...) → gibt ersten wahren Wert zurück, sonst nil
func evalOr(args *Cell, env *Env) (*Cell, error) {
  for args != nil && args.Type == LIST {
    val, err := Eval(args.Car, env)
    if err != nil { return nil, err }
    if isTruthy(val) { return val, nil }  // Kurzschluss!
    args = args.Cdr
  }
  return MakeNil(), nil
}

// not: (not x) → t wenn x falsch, sonst nil
func evalNot(args *Cell, env *Env) (*Cell, error) {
  val, err := Eval(args.Car, env)
  if err != nil { return nil, err }
  if isTruthy(val) { return MakeNil(), nil }
  return MakeAtom("t"), nil
}

// defmacro: (defmacro name (params) body)
// Wie defun, aber speichert MACRO statt LIST
func evalDefmacro(args *Cell, env *Env) (*Cell, error) {
  name := args.Car.Val
  lam  := makeLambda(args.Cdr.Car, wrapBegin(args.Cdr.Cdr), env)
  lam.Type = MACRO   // ← einziger Unterschied zu defun!
  env.Set(name, lam)
  return MakeAtom(name), nil
}

// case: (case key-expr ((val1 val2) result1) (else result3) ...)
// Syntaktischer Zucker fuer cond mit strukturellem Vergleich.
// Gibt Tripel zurück, damit der Eval-Loop TCO-fähig bleibt (case ist Tail).
func evalCase(args *Cell, env *Env) (*Cell, *Env, error) {
  if args == nil || args.Type != LIST {
    return nil, nil, fmt.Errorf("case: Syntax: (case key-expr clause...)")
  }
  key, err := Eval(args.Car, env)
  if err != nil { return nil, nil, err }

  for clauses := args.Cdr; clauses != nil && clauses.Type == LIST; clauses = clauses.Cdr {
    clause := clauses.Car
    if clause == nil || clause.Type != LIST { continue }

    test := clause.Car
    isElse := test.Type == ATOM && (test.Val == "else" || test.Val == "t")

    match := false
    if !isElse && test.Type == LIST {
      // Liste von Werten: ((a b c) result)
      for vals := test; vals != nil && vals.Type == LIST; vals = vals.Cdr {
        if cellEqual(key, vals.Car) { match = true; break }
      }
    } else if !isElse {
      // Einzelner Wert: (a result)
      if cellEqual(key, test) { match = true }
    }

    if isElse || match {
      body := clause.Cdr
      if body == nil || body.Type != LIST { return MakeNil(), env, nil }
      // Evaluiere alle Ausdruecke ausser dem letzten
      for body.Cdr != nil && body.Cdr.Type == LIST {
        _, err := Eval(body.Car, env)
        if err != nil { return nil, nil, err }
        body = body.Cdr
      }
      return body.Car, env, nil
    }
  }
  return MakeNil(), env, nil
}
