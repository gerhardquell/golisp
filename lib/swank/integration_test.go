//**********************************************************************
//  lib/swank/integration_test.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260618
//**********************************************************************
// End-to-end test für SWANK connection-info.
//**********************************************************************

package swank

import (
  "net"
  "strings"
  "testing"
  "time"

  "golisp/lib"
)

func TestSwankServerConnectionInfo(t *testing.T) {
  listener, err := net.Listen("tcp", "127.0.0.1:0")
  if err != nil {
    t.Fatalf("listen: %v", err)
  }
  defer listener.Close()

  go func() {
    for {
      conn, err := listener.Accept()
      if err != nil {
        return
      }
      go handleConn(conn)
    }
  }()

  conn, err := net.Dial("tcp", listener.Addr().String())
  if err != nil {
    t.Fatalf("dial: %v", err)
  }
  defer conn.Close()

  // Send connection-info request
  msg := lib.Cons(lib.MakeAtom(":emacs-rex"),
    lib.Cons(lib.Cons(lib.MakeAtom("swank:connection-info"), lib.MakeNil()),
      lib.Cons(lib.MakeNil(),
        lib.Cons(lib.MakeAtom("t"),
          lib.Cons(lib.MakeNum(1), lib.MakeNil())))))
  if err := writeFrame(conn, msg); err != nil {
    t.Fatalf("writeFrame: %v", err)
  }

  // Set read deadline to avoid hanging
  conn.SetReadDeadline(time.Now().Add(2 * time.Second))

  resp, err := readFrame(conn)
  if err != nil {
    t.Fatalf("readFrame: %v", err)
  }
  s := resp.String()
  if !strings.Contains(s, ":return") || !strings.Contains(s, "GoLisp") {
    t.Fatalf("unexpected response: %s", s)
  }
}
