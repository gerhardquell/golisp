package lib

import (
  "slices"
  "testing"
)

func TestSymbolsContainsBuiltins(t *testing.T) {
  env := BaseEnv()
  syms := env.Symbols()

  // nur Primitiven testen (Spezialformen wie defun/lambda leben im evalList-Switch, nicht in BaseEnv)
  for _, want := range []string{"+", "-", "*", "/", "=", "<", ">", ">=", "<=", "car", "cdr", "cons", "println", "gensym"} {
    if !slices.Contains(syms, want) {
      t.Errorf("Symbols() fehlt: %q", want)
    }
  }
}

func TestSymbolsIncludesUserDefined(t *testing.T) {
  env := BaseEnv()
  env.Set("meine-fn", MakeAtom("t"))
  syms := env.Symbols()
  if !slices.Contains(syms, "meine-fn") {
    t.Error("Symbols() enthält keine user-definierten Symbole")
  }
}
