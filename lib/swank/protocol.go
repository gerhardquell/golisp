//**********************************************************************
//  lib/swank/protocol.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260301
//**********************************************************************
// Protokoll-Handler für SWANK-ähnliche S-Expression-RPC
//**********************************************************************

package swank

import (
	"fmt"
	"os"
	"strings"

	"golisp/lib"
)

// dispatch leitet Requests an die entsprechenden Handler weiter
func dispatch(method string, params *lib.Cell, env *lib.Env) (*lib.Cell, error) {
	switch method {
	case "ping":
		return handlePing(params, env)
	case "eval":
		return handleEval(params, env)
	case "eval-return":
		return handleEvalReturn(params, env)
	case "complete":
		return handleComplete(params, env)
	case "symbols":
		return handleSymbols(params, env)
	case "describe":
		return handleDescribe(params, env)
	case "load-file":
		return handleLoadFile(params, env)
	case "disconnect":
		return handleDisconnect(params, env)
	case "macroexpand":
		return handleMacroexpand(params, env)
	default:
		return nil, fmt.Errorf("unbekannte Methode: %s", method)
	}
}

// handlePing: Health Check
func handlePing(params *lib.Cell, env *lib.Env) (*lib.Cell, error) {
	return lib.MakeString("pong"), nil
}

// handleEval: Lisp-Code auswerten, Ergebnis als String
func handleEval(params *lib.Cell, env *lib.Env) (*lib.Cell, error) {
	if params == nil || params.Type == lib.NIL {
		return nil, fmt.Errorf("eval braucht mindestens einen Parameter")
	}

	// Erster Parameter: Code-String
	codeCell := params.Car
	if codeCell == nil || codeCell.Type != lib.STRING {
		return nil, fmt.Errorf("eval: erster Parameter muss String sein")
	}
	code := codeCell.Val

	// Parsen und auswerten
	cell, err := lib.Read(code)
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}

	result, err := lib.Eval(cell, env)
	if err != nil {
		return nil, fmt.Errorf("eval: %w", err)
	}

	// Ergebnis als String zurückgeben
	return lib.MakeString(result.String()), nil
}

// handleEvalReturn: Lisp-Code auswerten, Ergebnis als Cell-Struktur
func handleEvalReturn(params *lib.Cell, env *lib.Env) (*lib.Cell, error) {
	if params == nil || params.Type == lib.NIL {
		return nil, fmt.Errorf("eval-return braucht mindestens einen Parameter")
	}

	// Erster Parameter: Code-String
	codeCell := params.Car
	if codeCell == nil || codeCell.Type != lib.STRING {
		return nil, fmt.Errorf("eval-return: erster Parameter muss String sein")
	}
	code := codeCell.Val

	// Parsen und auswerten
	cell, err := lib.Read(code)
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}

	result, err := lib.Eval(cell, env)
	if err != nil {
		return nil, fmt.Errorf("eval: %w", err)
	}

	// Ergebnis als Cell-Struktur zurückgeben
	return result, nil
}

// handleComplete: Autocomplete für Symbole
func handleComplete(params *lib.Cell, env *lib.Env) (*lib.Cell, error) {
	if params == nil || params.Type == lib.NIL {
		return nil, fmt.Errorf("complete braucht mindestens einen Parameter")
	}

	// Erster Parameter: Prefix
	prefixCell := params.Car
	if prefixCell == nil || prefixCell.Type != lib.STRING {
		return nil, fmt.Errorf("complete: erster Parameter muss String sein")
	}
	prefix := prefixCell.Val

	// Alle Symbole holen
	symbols := env.Symbols()

	// Passende Symbole finden
	var matches []*lib.Cell
	for _, sym := range symbols {
		if strings.HasPrefix(sym, prefix) {
			doc := getDoc(sym, env)
			// Paar: (symbol . doc)
			pair := lib.Cons(lib.MakeAtom(sym), lib.MakeString(doc))
			matches = append(matches, pair)
		}
	}

	return lib.SliceToCell(matches), nil
}

// handleSymbols: Liste aller definierten Symbole
func handleSymbols(params *lib.Cell, env *lib.Env) (*lib.Cell, error) {
	symbols := env.Symbols()

	var cells []*lib.Cell
	for _, sym := range symbols {
		cells = append(cells, lib.MakeAtom(sym))
	}

	return lib.SliceToCell(cells), nil
}

// handleDescribe: Dokumentation für ein Symbol
func handleDescribe(params *lib.Cell, env *lib.Env) (*lib.Cell, error) {
	if params == nil || params.Type == lib.NIL {
		return nil, fmt.Errorf("describe braucht mindestens einen Parameter")
	}

	// Symbol-Name
	var symName string
	symCell := params.Car

	if symCell.Type == lib.STRING {
		symName = symCell.Val
	} else if symCell.Type == lib.ATOM {
		symName = symCell.Val
	} else {
		return nil, fmt.Errorf("describe: Parameter muss String oder Symbol sein")
	}

	doc := getDoc(symName, env)
	return lib.MakeString(doc), nil
}

// handleLoadFile: Datei laden
func handleLoadFile(params *lib.Cell, env *lib.Env) (*lib.Cell, error) {
	if params == nil || params.Type == lib.NIL {
		return nil, fmt.Errorf("load-file braucht mindestens einen Parameter")
	}

	// Dateipfad
	pathCell := params.Car
	if pathCell == nil || pathCell.Type != lib.STRING {
		return nil, fmt.Errorf("load-file: erster Parameter muss String sein")
	}
	path := pathCell.Val

	// Datei lesen
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("load-file: %w", err)
	}

	// Code auswerten
	_, err = lib.LoadString(string(data), env)
	if err != nil {
		return nil, fmt.Errorf("load-file: %w", err)
	}

	return lib.MakeString(fmt.Sprintf("loaded: %s", path)), nil
}

// handleDisconnect: Verbindung ordnungsgemäß beenden
func handleDisconnect(params *lib.Cell, env *lib.Env) (*lib.Cell, error) {
	// Gibt nil zurück, signalisiert dem Client dass die Verbindung geschlossen wird
	return lib.MakeNil(), nil
}

// handleMacroexpand: Makro expandieren
func handleMacroexpand(params *lib.Cell, env *lib.Env) (*lib.Cell, error) {
	if params == nil || params.Type == lib.NIL {
		return nil, fmt.Errorf("macroexpand braucht mindestens einen Parameter")
	}

	// Code-String
	codeCell := params.Car
	if codeCell == nil || codeCell.Type != lib.STRING {
		return nil, fmt.Errorf("macroexpand: erster Parameter muss String sein")
	}
	code := codeCell.Val

	// Parsen
	cell, err := lib.Read(code)
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}

	// Als Liste prüfen
	if cell == nil || cell.Type != lib.LIST || cell.Car == nil {
		return lib.MakeString(code), nil
	}

	// Prüfen ob erstes Element ein Makro ist
	fn, err := lib.Eval(cell.Car, env)
	if err != nil {
		// Symbol nicht gefunden, unverändert zurückgeben
		return lib.MakeString(cell.String()), nil
	}

	// Wenn es ein Makro ist, expandieren
	if lib.IsMacro(fn) {
		// Macroexpand über eval
		expandExpr := lib.Cons(lib.MakeAtom("macroexpand"), lib.Cons(cell, lib.MakeNil()))
		result, err := lib.Eval(expandExpr, env)
		if err != nil {
			return lib.MakeString(cell.String()), nil
		}
		return lib.MakeString(result.String()), nil
	}

	// Kein Makro, unverändert zurückgeben
	return lib.MakeString(cell.String()), nil
}

// getDoc liefert Dokumentation für ein Symbol
func getDoc(sym string, env *lib.Env) string {
	val, err := env.Get(sym)
	if err != nil {
		return "unbekanntes Symbol"
	}

	switch val.Type {
	case lib.FUNC:
		return "Eingebaute Funktion"
	case lib.LIST:
		if val.Env != nil {
			return "Lambda/Closure"
		}
		return "Liste"
	case lib.MACRO:
		return "Makro"
	case lib.NUMBER:
		return "Zahl"
	case lib.STRING:
		return "String"
	case lib.ATOM:
		return "Symbol"
	case lib.NIL:
		return "Nil/leere Liste"
	default:
		return "Unbekannter Typ"
	}
}
