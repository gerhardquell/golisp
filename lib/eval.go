//**********************************************************************
//  lib/eval.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260223
//**********************************************************************

package lib

import (
  "fmt"
  "os"
  "strings"
  "sync"
)

func Eval(expr *Cell, env *Env) (*Cell, error) {
  if expr == nil { return MakeNil(), nil }

  switch expr.Type {
  case NIL, NUMBER, STRING, FUNC: return expr, nil

  case ATOM:
    return env.Get(expr.Val)

  case LIST:
    return evalList(expr, env)
  }
  return nil, fmt.Errorf("eval: unbekannter Typ")
}

func evalList(expr *Cell, env *Env) (*Cell, error) {
  if expr.Car == nil { return MakeNil(), nil }

  if expr.Car.Type == ATOM {
    switch expr.Car.Val {
    case "quote":  return expr.Cdr.Car, nil
    case "if":     return evalIf(expr.Cdr, env)
    case "define": return evalDefine(expr.Cdr, env)
    case "defun":    return evalDefun(expr.Cdr, env)
    case "defmacro": return evalDefmacro(expr.Cdr, env)
    case "parfunc":  return evalParfunc(expr.Cdr, env)
    case "eval":     return evalEval(expr.Cdr, env)
    case "lock":     return evalLock(expr.Cdr, env)
    case "lambda": return evalLambda(expr.Cdr, env)
    case "let":    return evalLet(expr.Cdr, env)
    case "begin":  return evalBegin(expr.Cdr, env)
    case "set!":    return evalSet(expr.Cdr, env)
    case "mapcar":  return evalMapcar(expr.Cdr, env)
    case "load":    return evalLoad(expr.Cdr, env)
    case "and":     return evalAnd(expr.Cdr, env)
    case "or":      return evalOr(expr.Cdr, env)
    case "not":          return evalNot(expr.Cdr, env)
    case "cond":         return evalCond(expr.Cdr, env)
    case "quasiquote":   return evalQuasiquote(expr.Cdr, env)
    case "unquote":      return nil, fmt.Errorf("unquote: außerhalb von quasiquote")
    case "unquote-splice": return nil, fmt.Errorf("unquote-splice: außerhalb von quasiquote")
    }
  }

  fn, err := Eval(expr.Car, env)
  if err != nil { return nil, err }

  // Makro: Argumente NICHT auswerten, expandieren, dann Ergebnis auswerten
  if fn.Type == MACRO {
    expanded, err := applyLambda(fn, cellToSlice(expr.Cdr))
    if err != nil { return nil, err }
    return Eval(expanded, env)
  }

  args, err := evalArgs(expr.Cdr, env)
  if err != nil { return nil, err }
  return apply(fn, args)
}

func evalArgs(args *Cell, env *Env) ([]*Cell, error) {
  var result []*Cell
  for args != nil && args.Type == LIST {
    val, err := Eval(args.Car, env)
    if err != nil { return nil, err }
    result = append(result, val)
    args = args.Cdr
  }
  return result, nil
}

func apply(fn *Cell, args []*Cell) (*Cell, error) {
  switch fn.Type {
  case FUNC: return fn.Fn(args)
  case LIST: return applyLambda(fn, args)
  default:   return nil, fmt.Errorf("apply: '%s' ist keine Funktion", fn)
  }
}

// Lambda-Struktur: Cell{Type:LIST, Car:params, Cdr:body, Env:closureEnv}
func applyLambda(lambda *Cell, args []*Cell) (*Cell, error) {
  closureEnv := lambda.Env.(*Env)
  params     := lambda.Car
  body       := lambda.Cdr
  localEnv   := NewEnv(closureEnv)

  p, i := params, 0
  for p != nil && p.Type == LIST {
    if i >= len(args) {
      return nil, fmt.Errorf("lambda: zu wenig Argumente")
    }
    localEnv.Set(p.Car.Val, args[i])
    p = p.Cdr
    i++
  }
  return Eval(body, localEnv)
}

func evalIf(args *Cell, env *Env) (*Cell, error) {
  cond, err := Eval(args.Car, env)
  if err != nil { return nil, err }
  if isTruthy(cond) { return Eval(args.Cdr.Car, env) }
  if args.Cdr.Cdr != nil && args.Cdr.Cdr.Type == LIST {
    return Eval(args.Cdr.Cdr.Car, env)
  }
  return MakeNil(), nil
}

func evalDefine(args *Cell, env *Env) (*Cell, error) {
  name := args.Car.Val
  val, err := Eval(args.Cdr.Car, env)
  if err != nil { return nil, err }
  env.Set(name, val)
  return MakeAtom(name), nil
}

func evalDefun(args *Cell, env *Env) (*Cell, error) {
  name := args.Car.Val
  lam  := makeLambda(args.Cdr.Car, args.Cdr.Cdr.Car, env)
  env.Set(name, lam)
  return MakeAtom(name), nil
}

func evalLambda(args *Cell, env *Env) (*Cell, error) {
  return makeLambda(args.Car, args.Cdr.Car, env), nil
}

func makeLambda(params, body *Cell, env *Env) *Cell {
  return &Cell{Type: LIST, Car: params, Cdr: body, Env: env}
}

func evalLet(args *Cell, env *Env) (*Cell, error) {
  localEnv := NewEnv(env)
  bindings, body := args.Car, args.Cdr.Car

  for bindings != nil && bindings.Type == LIST {
    b := bindings.Car
    val, err := Eval(b.Cdr.Car, env)
    if err != nil { return nil, err }
    localEnv.Set(b.Car.Val, val)
    bindings = bindings.Cdr
  }
  return Eval(body, localEnv)
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

func isTruthy(c *Cell) bool {
  return c != nil && c.Type != NIL
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

// load: (load "datei.lisp") → liest und wertet alle Ausdrücke aus
func evalLoad(args *Cell, env *Env) (*Cell, error) {
  filename, err := Eval(args.Car, env)
  if err != nil { return nil, err }

  data, err := os.ReadFile(filename.Val)
  if err != nil { return nil, fmt.Errorf("load: '%s' nicht gefunden", filename.Val) }

  src := strings.TrimSpace(string(data))
  var result *Cell

  // Mehrere Ausdrücke in der Datei nacheinander auswerten
  r := NewReader(src)
  for {
    r.skipWS()
    if r.pos >= len(r.src) { break }

    expr, err := r.readExpr()
    if err != nil { return nil, fmt.Errorf("load %s: %v", filename.Val, err) }

    result, err = Eval(expr, env)
    if err != nil { return nil, fmt.Errorf("load %s: %v", filename.Val, err) }
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

// cellToSlice wandelt eine Lisp-Liste in einen Go-Slice um (ohne Eval!)
func cellToSlice(args *Cell) []*Cell {
  var result []*Cell
  for args != nil && args.Type == LIST {
    result = append(result, args.Car)
    args = args.Cdr
  }
  return result
}

// defmacro: (defmacro name (params) body)
// Wie defun, aber speichert MACRO statt LIST
func evalDefmacro(args *Cell, env *Env) (*Cell, error) {
  name := args.Car.Val
  lam  := makeLambda(args.Cdr.Car, args.Cdr.Cdr.Car, env)
  lam.Type = MACRO   // ← einziger Unterschied zu defun!
  env.Set(name, lam)
  return MakeAtom(name), nil
}

// parfunc: (parfunc ergebnis expr1 expr2 ...)
// Wertet alle Ausdrücke parallel aus, sammelt Ergebnisse als Liste
func evalParfunc(args *Cell, env *Env) (*Cell, error) {
  if args == nil || args.Type != LIST {
    return nil, fmt.Errorf("parfunc: Syntax: (parfunc name expr...)")
  }

  // erstes Argument: Name für die Ergebnisliste
  resultName := args.Car.Val
  exprs      := args.Cdr

  // Ausdrücke sammeln
  var exprList []*Cell
  for e := exprs; e != nil && e.Type == LIST; e = e.Cdr {
    exprList = append(exprList, e.Car)
  }
  if len(exprList) == 0 { return MakeNil(), nil }

  // Parallel auswerten
  results := make([]*Cell, len(exprList))
  errors  := make([]error,  len(exprList))
  var wg sync.WaitGroup

  for i, expr := range exprList {
    wg.Add(1)
    go func(idx int, e *Cell) {
      defer wg.Done()
      // jede Goroutine bekommt eigene Env-Kopie (read-only reicht hier)
      results[idx], errors[idx] = Eval(e, env)
    }(i, expr)
  }
  wg.Wait()

  // Fehler prüfen
  for i, err := range errors {
    if err != nil {
      return nil, fmt.Errorf("parfunc[%d]: %v", i, err)
    }
  }

  // Ergebnisse als Lisp-Liste aufbauen
  result := MakeNil()
  for i := len(results) - 1; i >= 0; i-- {
    result = Cons(results[i], result)
  }

  // In env speichern
  env.Set(resultName, result)
  return result, nil
}

// lock: (lock mu expr1 expr2 ...) → atomar ausführen
func evalLock(args *Cell, env *Env) (*Cell, error) {
  if args == nil || args.Type != LIST {
    return nil, fmt.Errorf("lock: Syntax: (lock mutex expr...)")
  }

  muCell, err := Eval(args.Car, env)
  if err != nil { return nil, err }

  gm, err := getMutex(muCell)
  if err != nil { return nil, err }

  gm.mu.Lock()
  defer gm.mu.Unlock()

  return evalBegin(args.Cdr, env)
}

// cond: (cond (test body...) (test body...) (t body...))
func evalCond(clauses *Cell, env *Env) (*Cell, error) {
  for c := clauses; c != nil && c.Type == LIST; c = c.Cdr {
    clause := c.Car
    if clause == nil || clause.Type != LIST {
      return nil, fmt.Errorf("cond: Klausel muss Liste sein")
    }
    test := clause.Car
    if test.Type == ATOM && (test.Val == "t" || test.Val == "else") {
      return evalBegin(clause.Cdr, env)
    }
    val, err := Eval(test, env)
    if err != nil { return nil, err }
    if isTruthy(val) {
      return evalBegin(clause.Cdr, env)
    }
  }
  return MakeNil(), nil
}

// quasiquote: `expr → wertet unquote/unquote-splice innerhalb aus
func evalQuasiquote(args *Cell, env *Env) (*Cell, error) {
  if args == nil || args.Type != LIST {
    return nil, fmt.Errorf("quasiquote: 1 Argument nötig")
  }
  return evalQQ(args.Car, env, 1)
}

func evalQQ(expr *Cell, env *Env, depth int) (*Cell, error) {
  if expr == nil { return MakeNil(), nil }
  if expr.Type != LIST { return expr, nil }

  if expr.Car != nil && expr.Car.Type == ATOM {
    switch expr.Car.Val {
    case "quasiquote":
      inner, err := evalQQ(expr.Cdr.Car, env, depth+1)
      if err != nil { return nil, err }
      return Cons(MakeAtom("quasiquote"), Cons(inner, MakeNil())), nil
    case "unquote":
      if depth == 1 { return Eval(expr.Cdr.Car, env) }
      inner, err := evalQQ(expr.Cdr.Car, env, depth-1)
      if err != nil { return nil, err }
      return Cons(MakeAtom("unquote"), Cons(inner, MakeNil())), nil
    case "unquote-splice":
      if depth == 1 {
        return nil, fmt.Errorf("unquote-splice: nur in Liste erlaubt")
      }
      inner, err := evalQQ(expr.Cdr.Car, env, depth-1)
      if err != nil { return nil, err }
      return Cons(MakeAtom("unquote-splice"), Cons(inner, MakeNil())), nil
    }
  }
  return evalQQList(expr, env, depth)
}

func evalQQList(lst *Cell, env *Env, depth int) (*Cell, error) {
  if lst == nil || lst.Type != LIST { return lst, nil }
  car := lst.Car

  // unquote-splice: ,@expr → gesplicete Liste einfügen
  if car != nil && car.Type == LIST &&
    car.Car != nil && car.Car.Type == ATOM && car.Car.Val == "unquote-splice" {
    if depth == 1 {
      spliced, err := Eval(car.Cdr.Car, env)
      if err != nil { return nil, err }
      rest, err := evalQQList(lst.Cdr, env, depth)
      if err != nil { return nil, err }
      return appendList(spliced, rest), nil
    }
  }

  processedCar, err := evalQQ(car, env, depth)
  if err != nil { return nil, err }
  processedCdr, err := evalQQList(lst.Cdr, env, depth)
  if err != nil { return nil, err }
  return Cons(processedCar, processedCdr), nil
}

func appendList(lst, tail *Cell) *Cell {
  if lst == nil || lst.Type == NIL { return tail }
  if lst.Type != LIST { return tail }
  return Cons(lst.Car, appendList(lst.Cdr, tail))
}

// eval: (eval ausdruck) → wertet einen Ausdruck nochmal aus
// Beispiel: (eval (list '+ 1 2)) → 3
//           (eval (read "(+ 1 2)")) → 3
func evalEval(args *Cell, env *Env) (*Cell, error) {
  if args == nil || args.Type != LIST {
    return nil, fmt.Errorf("eval: 1 Argument nötig")
  }
  // Argument erst auswerten (z.B. Variable oder read-Ergebnis)
  expr, err := Eval(args.Car, env)
  if err != nil { return nil, err }
  // dann nochmal auswerten
  return Eval(expr, env)
}
