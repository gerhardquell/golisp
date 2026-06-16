//**********************************************************************
//  lib/reader_test.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260616
//**********************************************************************
// Charakterisierungstests für den Reader.
// Zweck: Sicherheitsnetz vor dem eval.go-Split (Todo #1).
// Sie halten das IST-Verhalten fest, nicht ein gewünschtes SOLL –
// Abweichungen sind Bugs, die explizit markiert werden.
//**********************************************************************

package lib

import "testing"

// assertRead prüft, dass src zu dem Cell-Baum parst, dessen
// String-Repräsentation want entspricht. Cell.String() ist die
// kanonische Form: sie ist unabhängig von Pointer-Identität und
// deckt Strukturfehler (falsche Type, kaputte Cons-Kette) zuverlässig auf.
func assertRead(t *testing.T, src, want string) {
  t.Helper()
  got, err := Read(src)
  if err != nil {
    t.Fatalf("Read(%q) Fehler: %v", src, err)
  }
  if got.String() != want {
    t.Errorf("Read(%q) = %q, want %q", src, got.String(), want)
  }
}

// assertReadErr prüft, dass src einen Fehler liefert.
func assertReadErr(t *testing.T, src string) {
  t.Helper()
  _, err := Read(src)
  if err == nil {
    t.Errorf("Read(%q) sollte Fehler geben, lieferte aber nil", src)
  }
}

func TestReadAtoms(t *testing.T) {
  cases := []struct{ src, want string }{
    {"foo", "foo"},
    {"+", "+"},
    {"-", "-"},            // "-" ist Symbol, keine Zahl (ParseFloat schlägt fehl)
    {"a-b", "a-b"},
    {"car", "car"},
  }
  for _, c := range cases {
    assertRead(t, c.src, c.want)
  }
}

func TestReadNumbers(t *testing.T) {
  cases := []struct{ src, want string }{
    {"42", "42"},
    {"3.14", "3.14"},
    {"0", "0"},
    {"-5", "-5"},
    {"100", "100"},
  }
  for _, c := range cases {
    assertRead(t, c.src, c.want)
  }
}

func TestReadStrings(t *testing.T) {
  cases := []struct{ src, want string }{
    {`"hallo"`, `"hallo"`},
    {`"hallo welt"`, `"hallo welt"`},
    {`"mit\nnewline"`, `"mit\nnewline"`},   // \n → echter Zeilenumbruch
    {`"tab\there"`, `"tab\there"`},
    {`"quote\"inside"`, `"quote\"inside"`},
    {`"back\\slash"`, `"back\\slash"`},
    {`""`, `""`},                            // leerer String
  }
  for _, c := range cases {
    assertRead(t, c.src, c.want)
  }
}

func TestReadLists(t *testing.T) {
  cases := []struct{ src, want string }{
    {"(a b c)", "(a b c)"},
    {"()", "()"},                        // leere Liste → Singleton-Nil (Stringer: "()")
    {"(1 2 3)", "(1 2 3)"},
    {"(a (b c) d)", "(a (b c) d)"},      // Verschachtelung
    {"(  a   b  )", "(a b)"},            // Whitespace toleriert
    {"((x))", "((x))"},                  // tiefere Verschachtelung
  }
  for _, c := range cases {
    assertRead(t, c.src, c.want)
  }
}

func TestReadNil(t *testing.T) {
  // nil/NIL → Singleton-Nil-Cell (Type: NIL), Stringer rendert "()".
  // eq-Pointer-Gleichheit mit (list) ist gewollt (siehe CLAUDE.md).
  assertRead(t, "nil", "()")
  assertRead(t, "NIL", "()")
  // Gemischte Case wie "Nil"/"nIl" sind Symbole (IST-Verhalten).
  assertRead(t, "Nil", "Nil")
  assertRead(t, "nIl", "nIl")
}

func TestReadQuote(t *testing.T) {
  assertRead(t, "'x", "(quote x)")
  assertRead(t, "'(a b)", "(quote (a b))")
  assertRead(t, "''x", "(quote (quote x))")  // geschachteltes Quote
}

func TestReadQuasiquote(t *testing.T) {
  assertRead(t, "`x", "(quasiquote x)")
  assertRead(t, ",x", "(unquote x)")
  assertRead(t, ",@x", "(unquote-splice x)")
  assertRead(t, "`(a ,b)", "(quasiquote (a (unquote b)))")
}

func TestReadDispatch(t *testing.T) {
  assertRead(t, "#'foo", "(function foo)")
  assertReadErr(t, "#x")   // unbekanntes Dispatch-Zeichen
  assertReadErr(t, "#")    // EOF nach #
}

func TestReadDottedPair(t *testing.T) {
  // (a . b) → Cons(a, b)
  got, err := Read("(a . b)")
  if err != nil {
    t.Fatalf("Read(\"(a . b)\") Fehler: %v", err)
  }
  if got.Type != LIST || got.Car.String() != "a" || got.Cdr.String() != "b" {
    t.Errorf("(a . b) = %q (Car=%q Cdr=%q), want Cons(a,b)",
      got.String(), got.Car.String(), got.Cdr.String())
  }
}

func TestReadComments(t *testing.T) {
  assertRead(t, "; nur Kommentar", "()")           // nur Kommentar → Nil → "()"
  assertRead(t, "a ; trailing", "a")
  assertRead(t, "(a ; Kommentar in Liste\n b)", "(a b)")
}

func TestReadWhitespaceTrim(t *testing.T) {
  assertRead(t, "  (a b)  ", "(a b)")
  assertRead(t, "\n\n42\n", "42")
}

func TestReadErrors(t *testing.T) {
  assertReadErr(t, "(a b")           // fehlendes ) / EOF in Liste
  assertReadErr(t, `"ungeschlossen`) // ungeschlossener String
  assertReadErr(t, "(")              // nacktes ( ohne Inhalt
  // Hinweis: "\" (Backslash außerhalb eines Strings) ist KEIN Fehler –
  // readAtomOrNum liest es als Symbol. Backslash ist nur innerhalb von
  // Strings als Escape special. IST-Verhalten, dokumentiert in TestReadAtoms.
  assertRead(t, `\`, `\`)
}

// TestReadNestedDeep sichert die TCO-unabhängige Reader-Rekursion:
// tiefe Listen dürfen den Stack nicht sprengen (Reader ist nicht
// trampoliniert, geht aber praktisch nie tief genug). Marker-Test.
func TestReadNestedDeep(t *testing.T) {
  // 50-fach verschachtelt: ((((( ... )))))
  src := ""
  for i := 0; i < 50; i++ {
    src += "("
  }
  src += "x"
  for i := 0; i < 50; i++ {
    src += ")"
  }
  got, err := Read(src)
  if err != nil {
    t.Fatalf("tief verschachtelt: %v", err)
  }
  // Kern muss "x" sein; 50 Hüllen drumherum.
  cur := got
  for i := 0; i < 50; i++ {
    if cur.Type != LIST || cur.Cdr.String() != "()" {
      t.Fatalf("Hülle %d kaputt: %q", i, cur.String())
    }
    cur = cur.Car
  }
  if cur.String() != "x" {
    t.Errorf("Kern = %q, want x", cur.String())
  }
}
