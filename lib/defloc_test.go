//**********************************************************************
//  lib/defloc_test.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260624
//**********************************************************************
// Tests für Definition-Registry (symbol -> file/line)
//**********************************************************************

package lib

import (
  "sync"
  "testing"
)

func TestRegisterAndLookupDefinition(t *testing.T) {
  ClearDefinitions()
  RegisterDefinition("foo", "/a/b.lisp", 7)
  loc, ok := LookupDefinition("foo")
  if !ok {
    t.Fatalf("foo nicht gefunden")
  }
  if loc.File != "/a/b.lisp" || loc.Line != 7 {
    t.Fatalf("got %+v", loc)
  }
}

func TestLookupUnknownDefinition(t *testing.T) {
  ClearDefinitions()
  _, ok := LookupDefinition("nope")
  if ok {
    t.Fatalf("nope sollte nicht gefunden werden")
  }
}

func TestConcurrentRegisterDefinition(t *testing.T) {
  ClearDefinitions()
  var wg sync.WaitGroup
  for i := 0; i < 50; i++ {
    wg.Add(1)
    go func(n int) {
      defer wg.Done()
      RegisterDefinition("c", "/c.lisp", n)
    }(i)
  }
  wg.Wait()
  _, ok := LookupDefinition("c")
  if !ok {
    t.Fatalf("c nach concurrent writes nicht gefunden")
  }
}
