//**********************************************************************
//  lib/swank/server.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260301
//**********************************************************************
// SWANK-ähnlicher Server für GoLisp
// Bietet TCP-basierte S-Expression-RPC Schnittstelle
//**********************************************************************

package swank

import (
	"bufio"
	"fmt"
	"net"
	"os"
	"strings"
	"sync"

	"golisp/lib"
)

// Server repräsentiert den golispd Server
type Server struct {
	listener net.Listener
	env      *lib.Env
	port     string
	host     string
	mu       sync.RWMutex
	running  bool
	clients  map[net.Conn]bool
}

// NewServer erstellt einen neuen Server
func NewServer(host, port string) *Server {
	return &Server{
		port:    port,
		host:    host,
		clients: make(map[net.Conn]bool),
	}
}

// Start initialisiert und startet den Server
func (s *Server) Start() error {
	// Gemeinsame Umgebung erstellen (lädt stdlib einmal)
	s.env = lib.BaseEnv()

	// Standardbibliothek laden
	if err := s.loadStdlib(); err != nil {
		return fmt.Errorf("server: stdlib laden fehlgeschlagen: %w", err)
	}

	// TCP Listener starten
	addr := s.host + ":" + s.port
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		return fmt.Errorf("server: kann nicht auf %s lauschen: %w", addr, err)
	}
	s.listener = listener
	s.running = true

	fmt.Fprintf(os.Stderr, "golispd läuft auf %s\n", addr)

	// Verbindungen akzeptieren
	for s.running {
		conn, err := listener.Accept()
		if err != nil {
			if s.running {
				fmt.Fprintf(os.Stderr, "server: accept fehlgeschlagen: %v\n", err)
			}
			continue
		}

		s.mu.Lock()
		s.clients[conn] = true
		s.mu.Unlock()

		// Jede Verbindung in eigener Goroutine behandeln
		go s.handleConnection(conn)
	}

	return nil
}

// Stop beendet den Server
func (s *Server) Stop() error {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.running = false

	// Alle Client-Verbindungen schließen
	for conn := range s.clients {
		conn.Close()
		delete(s.clients, conn)
	}

	if s.listener != nil {
		return s.listener.Close()
	}
	return nil
}

// loadStdlib lädt die zentrale Standardbibliothek (lib.LoadStdlib).
// Früher inline-String mit abgespeckter Teilmenge – das führte zu Drift
// zwischen CLI (volle stdlib.lisp) und Server (20/52 Funktionen). Jetzt
// eine gemeinsame Quelle via //go:embed in lib/stdlib.go.
func (s *Server) loadStdlib() error {
	return lib.LoadStdlib(s.env)
}

// handleConnection behandelt eine einzelne Client-Verbindung
func (s *Server) handleConnection(conn net.Conn) {
	defer func() {
		s.mu.Lock()
		delete(s.clients, conn)
		s.mu.Unlock()
		conn.Close()
	}()

	reader := bufio.NewReader(conn)

	for s.running {
		// Zeile lesen (S-Expression)
		line, err := reader.ReadString('\n')
		if err != nil {
			// Verbindung geschlossen oder Fehler
			return
		}

		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		// Request parsen und verarbeiten
		response, err := s.processRequest(line)
		if err != nil {
			response = s.makeErrorResponse(0, err.Error())
		}

		// Antwort senden
		_, err = fmt.Fprintln(conn, response)
		if err != nil {
			return
		}
	}
}

// processRequest parst und verarbeitet einen Request
func (s *Server) processRequest(line string) (string, error) {
	// S-Expression parsen
	cell, err := lib.Read(line)
	if err != nil {
		return "", fmt.Errorf("parse error: %w", err)
	}

	// Request-Format: (:id 1 :method "eval" :params (...))
	// Als Property-Liste parsen
	req, err := parseRequest(cell)
	if err != nil {
		return "", err
	}

	// Methode dispatch
	result, err := dispatch(req.Method, req.Params, s.env)
	if err != nil {
		return formatResponse(req.ID, "error", err.Error(), nil), nil
	}

	return formatResponse(req.ID, "ok", "", result), nil
}

// Request-Struktur
type request struct {
	ID     int
	Method string
	Params *lib.Cell
}

// parseRequest wandelt eine Cell in eine Request-Struktur um
func parseRequest(cell *lib.Cell) (*request, error) {
	req := &request{ID: 0, Method: "", Params: lib.MakeNil()}

	if cell == nil || cell.Type != lib.LIST {
		return nil, fmt.Errorf("request muss eine Liste sein")
	}

	// Property-Liste parsen: (:id 1 :method "eval" :params (...))
	lst := cell
	for lst != nil && lst.Type == lib.LIST && lst.Cdr != nil && lst.Cdr.Type == lib.LIST {
		key := lst.Car
		lst = lst.Cdr
		val := lst.Car
		lst = lst.Cdr

		if key == nil || key.Type != lib.ATOM {
			continue
		}

		switch key.Val {
		case ":id":
			if val != nil && val.Type == lib.NUMBER {
				req.ID = int(val.Num)
			}
		case ":method":
			if val != nil && val.Type == lib.STRING {
				req.Method = val.Val
			}
		case ":params":
			req.Params = val
		}
	}

	if req.Method == "" {
		return nil, fmt.Errorf("keine Methode angegeben")
	}

	return req, nil
}

// formatResponse formatiert eine Antwort als S-Expression
func formatResponse(id int, status, err string, result *lib.Cell) string {
	if status == "error" {
		return fmt.Sprintf("(:id %d :status \"%s\" :error \"%s\")", id, status, escapeString(err))
	}
	if result == nil {
		result = lib.MakeNil()
	}
	// Strings escapen um Multiline-Antworten zu unterstützen
	if result.Type == lib.STRING {
		return fmt.Sprintf("(:id %d :status \"%s\" :result \"%s\")", id, status, escapeString(result.Val))
	}
	return fmt.Sprintf("(:id %d :status \"%s\" :result %s)", id, status, result.String())
}

// makeErrorResponse erstellt eine Fehlerantwort
func (s *Server) makeErrorResponse(id int, msg string) string {
	return formatResponse(id, "error", msg, nil)
}

// escapeString escaped Newlines in Strings (aber nicht Quotes - die sind im Format-String)
func escapeString(s string) string {
	s = strings.ReplaceAll(s, "\\", "\\\\")
	s = strings.ReplaceAll(s, "\n", "\\n")
	s = strings.ReplaceAll(s, "\t", "\\t")
	// Quotes werden NICHT escapet - sie sind bereits durch Format-String "..." geschützt
	return s
}
