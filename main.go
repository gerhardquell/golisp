//**********************************************************************
//  main.go  - GoLisp REPL
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260224
//**********************************************************************

package main

import (
  "errors"
  "fmt"
  "os"
  "golisp/lib"
)

func test(env *lib.Env, s string) {
  cell, _ := lib.Read(s)
  result, err := lib.Eval(cell, env)
  if err != nil { fmt.Printf("ERR: %v\n", err); return }
  fmt.Printf("%-45s => %s\n", s, result)
}

func main() {
  env := lib.BaseEnv()

  // Testmodus: go run . -t
  if len(os.Args) > 1 && os.Args[1] == "-t" {
    fmt.Println("=== GoLisp Test ===")
    fmt.Println()
    test(env, "(+ 1 2)")
    test(env, "(defun fak (n) (if (= n 0) 1 (* n (fak (- n 1)))))")
    test(env, "(fak 6)")
    test(env, "(mapcar (lambda (x) (* x x)) (list 1 2 3 4 5))")
    test(env, "(parfunc r (* 6 7) (+ 100 23))")
    test(env, "r")
    test(env, "(atom (gensym))")
    test(env, "(= (gensym) (gensym))")
    test(env, `(catch (error "oops") (lambda (e) (string-append "caught: " e)))`)
    test(env, `(catch (+ 1 2) (lambda (e) "fehler"))`)
    test(env, `(defun add-and-show (x y) (define s (+ x y)) s)`)
    test(env, `(add-and-show 3 4)`)
    test(env, `((lambda (x) (define d (* x 2)) d) 5)`)
    test(env, `(>= 5 3)`)
    test(env, `(>= 3 3)`)
    test(env, `(>= 2 3)`)
    test(env, `(<= 2 3)`)
    test(env, `(<= 3 3)`)
    test(env, `(<= 5 3)`)
    return
  }

  // Datei laden: go run . script.lisp
  if len(os.Args) > 1 {
    cell, err := lib.Read(`(load "` + os.Args[1] + `")`)
    if err != nil { fmt.Println("ERR:", err); os.Exit(1) }
    if _, err := lib.Eval(cell, env); err != nil {
      var le *lib.LispError
      if errors.As(err, &le) {
        fmt.Println("ERR:", le.Msg)
      } else {
        fmt.Println("ERR:", err)
      }
      os.Exit(1)
    }
    return
  }

  // REPL
  fmt.Println("GoLisp 0.2  –  Ctrl+D oder (exit) zum Beenden")
  fmt.Println("Multiline: offene Klammern → Fortsetzung mit ..")
  rl := lib.NewReadline("golisp> ", env.Symbols)
  defer rl.Close()

  for {
    line, err := rl.Read()
    if err != nil { break }
    if line == "" { continue }
    if line == "(exit)" || line == "exit" { break }

    cell, err := lib.Read(line)
    if err != nil { fmt.Println("ERR read:", err); continue }
    result, err := lib.Eval(cell, env)
    if err != nil {
      var le *lib.LispError
      if errors.As(err, &le) {
        fmt.Println("ERR:", le.Msg)
      } else {
        fmt.Println("ERR:", err)
      }
      continue
    }
    fmt.Println("=>", result)
  }
  fmt.Println("Tschüss!")
}
