//**********************************************************************
//  lib/primitives.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260223
//**********************************************************************

package lib

import (
  "fmt"
  "sync/atomic"
)

// BaseEnv erstellt die globale Umgebung mit allen eingebauten Funktionen
func BaseEnv() *Env {
  env := NewEnv(nil)

  // Arithmetik
  env.Set("+",   makeFn(fnAdd))
  env.Set("-",   makeFn(fnSub))
  env.Set("*",   makeFn(fnMul))
  env.Set("/",   makeFn(fnDiv))

  // Vergleiche
  env.Set("=",   makeFn(fnEq))
  env.Set("<",   makeFn(fnLt))
  env.Set(">",   makeFn(fnGt))
  env.Set(">=",  makeFn(fnGe))
  env.Set("<=",  makeFn(fnLe))
  env.Set("equal?", makeFn(fnEqual))

  // Listen-Primitiven (die klassischen 7!)
  env.Set("car",  makeFn(fnCar))
  env.Set("cdr",  makeFn(fnCdr))
  env.Set("cons", makeFn(fnCons))
  env.Set("atom", makeFn(fnAtom))
  env.Set("null", makeFn(fnNull))
  env.Set("list", makeFn(fnList))

  // Ausgabe
  env.Set("print",   makeFn(fnPrint))
  env.Set("read",    makeFn(fnRead))
  env.Set("println", makeFn(fnPrintln))

  // Wahrheitswerte
  env.Set("t",   MakeAtom("t"))
  env.Set("nil", MakeNil())

  // apply, funcall
  env.Set("apply",   makeFn(fnApply))
  env.Set("funcall", makeFn(fnFuncall))

  // gensym
  env.Set("gensym", makeFn(fnGensym))

  // error
  env.Set("error", makeFn(fnError))

  // sigoREST
  RegisterSigo(env)

  // Goroutinen
  RegisterGoroutines(env)

  // Datei-I/O
  RegisterFileIO(env)

  // String-Funktionen
  RegisterStringFuncs(env)

  return env
}

func makeFn(f func([]*Cell) (*Cell, error)) *Cell {
  return &Cell{Type: FUNC, Fn: f}
}

// ---- Arithmetik ----

func fnAdd(args []*Cell) (*Cell, error) {
  n := 0.0
  for _, a := range args { n += a.Num }
  return MakeNum(n), nil
}

func fnSub(args []*Cell) (*Cell, error) {
  if len(args) == 0 { return MakeNum(0), nil }
  n := args[0].Num
  for _, a := range args[1:] { n -= a.Num }
  return MakeNum(n), nil
}

func fnMul(args []*Cell) (*Cell, error) {
  n := 1.0
  for _, a := range args { n *= a.Num }
  return MakeNum(n), nil
}

func fnDiv(args []*Cell) (*Cell, error) {
  if len(args) < 2 { return nil, fmt.Errorf("/: min. 2 Argumente") }
  if args[1].Num == 0 { return nil, fmt.Errorf("/: Division durch 0") }
  return MakeNum(args[0].Num / args[1].Num), nil
}

// ---- Vergleiche ----

func fnEq(args []*Cell) (*Cell, error) {
  if len(args) < 2 { return nil, fmt.Errorf("=: 2 Argumente nötig") }
  if args[0].Num == args[1].Num {
    return MakeAtom("t"), nil
  }
  return MakeNil(), nil
}

func fnLt(args []*Cell) (*Cell, error) {
  if len(args) < 2 { return nil, fmt.Errorf("<: 2 Argumente nötig") }
  if args[0].Num < args[1].Num { return MakeAtom("t"), nil }
  return MakeNil(), nil
}

func fnGt(args []*Cell) (*Cell, error) {
  if len(args) < 2 { return nil, fmt.Errorf(">: 2 Argumente nötig") }
  if args[0].Num > args[1].Num { return MakeAtom("t"), nil }
  return MakeNil(), nil
}

func fnGe(args []*Cell) (*Cell, error) {
  if len(args) < 2 { return nil, fmt.Errorf(">=: 2 Argumente nötig") }
  if args[0].Num >= args[1].Num { return MakeAtom("t"), nil }
  return MakeNil(), nil
}

func fnLe(args []*Cell) (*Cell, error) {
  if len(args) < 2 { return nil, fmt.Errorf("<=: 2 Argumente nötig") }
  if args[0].Num <= args[1].Num { return MakeAtom("t"), nil }
  return MakeNil(), nil
}

func fnEqual(args []*Cell) (*Cell, error) {
  if len(args) < 2 { return nil, fmt.Errorf("equal?: 2 Argumente nötig") }
  if cellEqual(args[0], args[1]) { return MakeAtom("t"), nil }
  return MakeNil(), nil
}

// cellEqual: struktureller Vergleich zweier Cells (rekursiv)
func cellEqual(a, b *Cell) bool {
  if a == nil && b == nil { return true }
  if a == nil || b == nil { return false }
  if a.Type != b.Type     { return false }
  switch a.Type {
  case NIL:    return true
  case NUMBER: return a.Num == b.Num
  case ATOM:   return a.Val == b.Val
  case STRING: return a.Val == b.Val
  case LIST:   return cellEqual(a.Car, b.Car) && cellEqual(a.Cdr, b.Cdr)
  }
  return false
}

// ---- Listen ----

func fnCar(args []*Cell) (*Cell, error) {
  if len(args) < 1 || args[0].Type != LIST {
    return nil, fmt.Errorf("car: Liste erwartet")
  }
  return args[0].Car, nil
}

func fnCdr(args []*Cell) (*Cell, error) {
  if len(args) < 1 || args[0].Type != LIST {
    return nil, fmt.Errorf("cdr: Liste erwartet")
  }
  if args[0].Cdr == nil { return MakeNil(), nil }
  return args[0].Cdr, nil
}

func fnCons(args []*Cell) (*Cell, error) {
  if len(args) < 2 { return nil, fmt.Errorf("cons: 2 Argumente nötig") }
  return Cons(args[0], args[1]), nil
}

func fnAtom(args []*Cell) (*Cell, error) {
  if len(args) < 1 { return nil, fmt.Errorf("atom: 1 Argument nötig") }
  if args[0].Type == LIST { return MakeNil(), nil }
  return MakeAtom("t"), nil
}

func fnNull(args []*Cell) (*Cell, error) {
  if len(args) < 1 { return nil, fmt.Errorf("null: 1 Argument nötig") }
  if args[0].Type == NIL { return MakeAtom("t"), nil }
  return MakeNil(), nil
}

func fnList(args []*Cell) (*Cell, error) {
  result := MakeNil()
  for i := len(args) - 1; i >= 0; i-- {
    result = Cons(args[i], result)
  }
  return result, nil
}

// ---- Ausgabe ----

func fnPrint(args []*Cell) (*Cell, error) {
  for _, a := range args { fmt.Print(a) }
  return MakeNil(), nil
}

func fnPrintln(args []*Cell) (*Cell, error) {
  for _, a := range args { fmt.Println(a) }
  return MakeNil(), nil
}

// apply: (apply fn arg1 ... liste) → fn auf alle Argumente anwenden
// Letztes Argument muss LIST sein, wird gespliced
func fnApply(args []*Cell) (*Cell, error) {
  if len(args) < 2 {
    return nil, fmt.Errorf("apply: 2+ Argumente nötig")
  }
  fn := args[0]
  combined := make([]*Cell, 0, len(args))
  // feste Argumente zwischen fn und der letzten Liste
  for i := 1; i < len(args)-1; i++ {
    combined = append(combined, args[i])
  }
  // letzte Liste entfalten
  last := args[len(args)-1]
  for lst := last; lst != nil && lst.Type == LIST; lst = lst.Cdr {
    combined = append(combined, lst.Car)
  }
  return apply(fn, combined)
}

// funcall: (funcall fn arg1 arg2 ...) → fn auf Argumente anwenden
func fnFuncall(args []*Cell) (*Cell, error) {
  if len(args) < 1 {
    return nil, fmt.Errorf("funcall: mindestens 1 Argument nötig")
  }
  return apply(args[0], args[1:])
}

// read: (read "string") → parst String zu Cell
func fnRead(args []*Cell) (*Cell, error) {
  if len(args) < 1 { return nil, fmt.Errorf("read: 1 Argument nötig") }
  return Read(args[0].Val)
}

// gensym: global-atomarer Zähler für eindeutige Symbole
var gensymCounter int64

func fnGensym(args []*Cell) (*Cell, error) {
  if len(args) != 0 {
    return nil, fmt.Errorf("gensym: keine Argumente erwartet")
  }
  n := atomic.AddInt64(&gensymCounter, 1)
  return MakeAtom(fmt.Sprintf("G__%d", n)), nil
}

// error: (error msg) → signalisiert Lisp-Laufzeitfehler
func fnError(args []*Cell) (*Cell, error) {
  if len(args) != 1 {
    return nil, fmt.Errorf("error: 1 Argument nötig")
  }
  return nil, &LispError{Msg: args[0]}
}
