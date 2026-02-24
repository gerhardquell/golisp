//**********************************************************************
//  main.go  - GoLisp REPL
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260224
//**********************************************************************

package main

import (
  _ "embed"
  "errors"
  "fmt"
  "os"
  "golisp/lib"
)

//go:embed stdlib.lisp
var stdlibSrc string

func test(env *lib.Env, s string) {
  cell, _ := lib.Read(s)
  result, err := lib.Eval(cell, env)
  if err != nil { fmt.Printf("ERR: %v\n", err); return }
  fmt.Printf("%-45s => %s\n", s, result)
}

func main() {
  env := lib.BaseEnv()

  // Standardbibliothek laden (eingebettet)
  if _, err := lib.LoadString(stdlibSrc, env); err != nil {
    fmt.Println("stdlib Fehler:", err)
    os.Exit(1)
  }

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
    // while
    test(env, `(define w 0)`)
    test(env, `(while (< w 3) (set! w (+ w 1)))`)
    test(env, `w`)
    // do: (do ((i 0 (+ i 1)) (s 0 (+ s i))) ((= i 5) s))  → 0+1+2+3+4 = 10
    test(env, `(do ((i 0 (+ i 1)) (s 0 (+ s i))) ((= i 5) s))`)
    // TCO: tiefe Rekursion ohne Stack-Overflow
    test(env, `(defun sum-acc (n acc) (if (= n 0) acc (sum-acc (- n 1) (+ acc n))))`)
    test(env, `(sum-acc 1000000 0)`)
    test(env, `(defun even? (n) (if (= n 0) t (odd?  (- n 1))))`)
    test(env, `(defun odd?  (n) (if (= n 0) () (even? (- n 1))))`)
    test(env, `(even? 1000000)`)
    // equal?
    test(env, `(equal? 1 1)`)
    test(env, `(equal? 1 2)`)
    test(env, `(equal? "a" "a")`)
    test(env, `(equal? (list 1 2) (list 1 2))`)
    test(env, `(equal? (list 1 2) (list 1 3))`)
    test(env, `(equal? (list 1 (list 2 3)) (list 1 (list 2 3)))`)
    test(env, `(equal? () ())`)
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
