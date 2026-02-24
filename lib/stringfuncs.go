//**********************************************************************
//  lib/stringfuncs.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260224
//**********************************************************************
// String-Primitiven für GoLisp
// string-length, string-append, substring,
// string-upcase, string-downcase, string->number, number->string
//**********************************************************************

package lib

import (
  "fmt"
  "strconv"
  "strings"
)

func RegisterStringFuncs(env *Env) {
  env.Set("string-length",   makeFn(fnStringLength))
  env.Set("string-append",   makeFn(fnStringAppend))
  env.Set("substring",       makeFn(fnSubstring))
  env.Set("string-upcase",   makeFn(fnStringUpcase))
  env.Set("string-downcase", makeFn(fnStringDowncase))
  env.Set("string->number",  makeFn(fnStringToNumber))
  env.Set("number->string",  makeFn(fnNumberToString))
}

func fnStringLength(args []*Cell) (*Cell, error) {
  if len(args) < 1 || args[0].Type != STRING {
    return nil, fmt.Errorf("string-length: String erwartet")
  }
  return MakeNum(float64(len([]rune(args[0].Val)))), nil
}

func fnStringAppend(args []*Cell) (*Cell, error) {
  var sb strings.Builder
  for i, a := range args {
    if a.Type != STRING {
      return nil, fmt.Errorf("string-append: Argument %d ist kein String", i+1)
    }
    sb.WriteString(a.Val)
  }
  return MakeStr(sb.String()), nil
}

func fnSubstring(args []*Cell) (*Cell, error) {
  if len(args) < 3 {
    return nil, fmt.Errorf("substring: 3 Argumente nötig (string start end)")
  }
  if args[0].Type != STRING {
    return nil, fmt.Errorf("substring: erstes Argument muss String sein")
  }
  runes := []rune(args[0].Val)
  start := int(args[1].Num)
  end   := int(args[2].Num)
  if start < 0 || end > len(runes) || start > end {
    return nil, fmt.Errorf("substring: Index außerhalb des Bereichs")
  }
  return MakeStr(string(runes[start:end])), nil
}

func fnStringUpcase(args []*Cell) (*Cell, error) {
  if len(args) < 1 || args[0].Type != STRING {
    return nil, fmt.Errorf("string-upcase: String erwartet")
  }
  return MakeStr(strings.ToUpper(args[0].Val)), nil
}

func fnStringDowncase(args []*Cell) (*Cell, error) {
  if len(args) < 1 || args[0].Type != STRING {
    return nil, fmt.Errorf("string-downcase: String erwartet")
  }
  return MakeStr(strings.ToLower(args[0].Val)), nil
}

func fnStringToNumber(args []*Cell) (*Cell, error) {
  if len(args) < 1 || args[0].Type != STRING {
    return nil, fmt.Errorf("string->number: String erwartet")
  }
  n, err := strconv.ParseFloat(args[0].Val, 64)
  if err != nil {
    return nil, fmt.Errorf("string->number: '%s' ist keine Zahl", args[0].Val)
  }
  return MakeNum(n), nil
}

func fnNumberToString(args []*Cell) (*Cell, error) {
  if len(args) < 1 || args[0].Type != NUMBER {
    return nil, fmt.Errorf("number->string: Zahl erwartet")
  }
  if args[0].Num == float64(int(args[0].Num)) {
    return MakeStr(fmt.Sprintf("%d", int(args[0].Num))), nil
  }
  return MakeStr(fmt.Sprintf("%g", args[0].Num)), nil
}
