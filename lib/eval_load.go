//**********************************************************************
//  lib/eval_load.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260616 (aufgespalten aus eval.go)
//**********************************************************************
// Laden von Lisp-Dateien: Pfad-Auflösung (GOLISP_PATH), evalLoad, LoadString.
//**********************************************************************

package lib

import (
  "fmt"
  "os"
  "strings"
)

var librarySearchPaths []string
var searchPathsInitialized bool

func initSearchPaths() []string {
  var paths []string
  paths = append(paths, "/lib/golib")
  paths = append(paths, "/usr/local/lib/golib")
  paths = append(paths, "./golib")
  if golispPath := os.Getenv("GOLISP_PATH"); golispPath != "" {
    for _, p := range strings.Split(golispPath, ":") {
      if p != "" {
        paths = append(paths, p)
      }
    }
  }
  return paths
}

func resolveLibraryPath(filename string) (string, error) {
  if !searchPathsInitialized {
    librarySearchPaths = initSearchPaths()
    searchPathsInitialized = true
  }
  if _, err := os.Stat(filename); err == nil {
    return filename, nil
  }
  for _, dir := range librarySearchPaths {
    fullPath := dir + "/" + filename
    if _, err := os.Stat(fullPath); err == nil {
      return fullPath, nil
    }
  }
  return "", fmt.Errorf("'%s' nicht gefunden in Suchpfaden", filename)
}

// load: (load "datei.lisp") → liest und wertet alle Ausdrücke aus
func evalLoad(args *Cell, env *Env) (*Cell, error) {
  filenameCell, err := Eval(args.Car, env)
  if err != nil { return nil, err }

  resolvedPath, err := resolveLibraryPath(filenameCell.Val)
  if err != nil {
    return nil, fmt.Errorf("load: %v", err)
  }

  data, err := os.ReadFile(resolvedPath)
  if err != nil {
    return nil, fmt.Errorf("load: '%s' nicht lesbar: %w", resolvedPath, err)
  }

  src := strings.TrimSpace(string(data))
  var result *Cell

  // Mehrere Ausdrücke in der Datei nacheinander auswerten
  r := NewReader(src)
  for {
    r.skipWS()
    if r.pos >= len(r.src) { break }

    expr, err := r.readExpr()
    if err != nil { return nil, fmt.Errorf("load %s: %w", resolvedPath, err) }

    if expr.Type == LIST {
      expr.SrcFile = resolvedPath
    }

    result, err = Eval(expr, env)
    if err != nil { return nil, fmt.Errorf("load %s: %w", resolvedPath, err) }
  }
  return result, nil
}

// LoadString: Mehrere Ausdrücke aus einem String auswerten
func LoadString(src string, env *Env) (*Cell, error) {
  src = strings.TrimSpace(src)
  var result *Cell
  r := NewReader(src)
  for {
    r.skipWS()
    if r.pos >= len(r.src) { break }
    expr, err := r.readExpr()
    if err != nil { return nil, fmt.Errorf("stdlib: %w", err) }
    result, err = Eval(expr, env)
    if err != nil { return nil, fmt.Errorf("stdlib: %w", err) }
  }
  return result, nil
}
