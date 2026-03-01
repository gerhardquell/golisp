//**********************************************************************
//  cmd/golisp-client/main.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260301
//**********************************************************************
// GoLisp Client - CLI-Client für den golispd Server
// Unterstützt eval, repl, complete, load-file
//**********************************************************************

package main

import (
	"bufio"
	"flag"
	"fmt"
	"net"
	"os"
	"strings"
)

// Client repräsentiert die Verbindung zum Server
type Client struct {
	conn   net.Conn
	reader *bufio.Reader
	host   string
	port   string
	msgID  int
}

// NewClient erstellt einen neuen Client
func NewClient(host, port string) *Client {
	return &Client{
		host:  host,
		port:  port,
		msgID: 0,
	}
}

// Connect stellt die Verbindung zum Server her
func (c *Client) Connect() error {
	addr := c.host + ":" + c.port
	conn, err := net.Dial("tcp", addr)
	if err != nil {
		return fmt.Errorf("kann nicht zu %s verbinden: %w", addr, err)
	}
	c.conn = conn
	c.reader = bufio.NewReader(conn)
	return nil
}

// Close schließt die Verbindung
func (c *Client) Close() {
	if c.conn != nil {
		c.conn.Close()
	}
}

// Send sendet einen Request und empfängt die Antwort
func (c *Client) Send(method string, params string) (string, error) {
	c.msgID++

	// Request formatieren
	request := fmt.Sprintf("(:id %d :method \"%s\" :params %s)\n", c.msgID, method, params)

	// Senden
	_, err := c.conn.Write([]byte(request))
	if err != nil {
		return "", fmt.Errorf("send fehlgeschlagen: %w", err)
	}

	// Antwort lesen
	response, err := c.reader.ReadString('\n')
	if err != nil {
		return "", fmt.Errorf("recv fehlgeschlagen: %w", err)
	}

	return strings.TrimSpace(response), nil
}

// Ping führt einen Health-Check durch
func (c *Client) Ping() error {
	resp, err := c.Send("ping", "()")
	if err != nil {
		return err
	}
	if strings.Contains(resp, "pong") {
		return nil
	}
	return fmt.Errorf("unexpected response: %s", resp)
}

// Eval wertet Lisp-Code aus
func (c *Client) Eval(code string) (string, error) {
	// Code escapen
	escaped := strings.ReplaceAll(code, "\\", "\\\\")
	escaped = strings.ReplaceAll(escaped, "\"", "\\\"")

	params := fmt.Sprintf("(\"%s\")", escaped)
	resp, err := c.Send("eval", params)
	if err != nil {
		return "", err
	}

	// Ergebnis extrahieren (:id X :status "ok" :result "...")
	return extractResult(resp), nil
}

// Complete führt Autocomplete durch
func (c *Client) Complete(prefix string) (string, error) {
	params := fmt.Sprintf("(\"%s\")", prefix)
	resp, err := c.Send("complete", params)
	if err != nil {
		return "", err
	}
	return extractResult(resp), nil
}

// LoadFile lädt eine Datei
func (c *Client) LoadFile(path string) (string, error) {
	params := fmt.Sprintf("(\"%s\")", path)
	resp, err := c.Send("load-file", params)
	if err != nil {
		return "", err
	}
	return extractResult(resp), nil
}

// Disconnect beendet die Verbindung ordnungsgemäß
func (c *Client) Disconnect() {
	c.Send("disconnect", "()")
}

// extractResult extrahiert das :result Feld aus der Antwort
func extractResult(response string) string {
	// Prüfe auf Fehler
	if strings.Contains(response, ":status \"error\"") {
		errIdx := strings.Index(response, ":error \"")
		if errIdx != -1 {
			start := errIdx + 8
			end := strings.Index(response[start:], "\"")
			if end != -1 {
				return "Error: " + response[start:start+end]
			}
		}
		return response
	}

	// Einfache Extraktion - suche nach :result
	idx := strings.Index(response, ":result ")
	if idx == -1 {
		return response
	}

	// Result-Wert extrahieren
	start := idx + 8 // Länge von ":result "
	remaining := response[start:]

	// Wenn es mit " beginnt, ist es ein String
	if strings.HasPrefix(remaining, "\"") {
		remaining = remaining[1:]
		end := strings.Index(remaining, "\"")
		if end != -1 {
			return remaining[:end]
		}
	}

	// Für S-Expressions: zähle Klammern um das vollständige Ergebnis zu finden
	if strings.HasPrefix(remaining, "(") {
		depth := 0
		inString := false
		escape := false
		for i, ch := range remaining {
			if escape {
				escape = false
				continue
			}
			if ch == '\\' && inString {
				escape = true
				continue
			}
			if ch == '"' && !escape {
				inString = !inString
				continue
			}
			if !inString {
				if ch == '(' {
					depth++
				} else if ch == ')' {
					depth--
					if depth == 0 {
						return remaining[:i+1]
					}
				}
			}
		}
	}

	// Fallback: bis zum Ende oder schließende Klammer
	end := strings.Index(remaining, ")")
	if end != -1 {
		return remaining[:end]
	}

	return remaining
}

// runREPL startet den interaktiven Modus
func runREPL(client *Client) {
	fmt.Println("GoLisp Client REPL")
	fmt.Printf("Verbunden mit %s:%s\n", client.host, client.port)
	fmt.Println("Befehle: :quit, :complete prefix, :load datei")
	fmt.Println()

	scanner := bufio.NewScanner(os.Stdin)
	multiline := ""
	openParens := 0

	for {
		if multiline == "" {
			fmt.Print("golisp> ")
		} else {
			fmt.Print("      > ")
		}

		if !scanner.Scan() {
			break
		}

		line := scanner.Text()

		// REPL-Befehle
		if multiline == "" {
			if line == ":quit" || line == ":q" {
				break
			}
			if strings.HasPrefix(line, ":complete ") {
				prefix := strings.TrimSpace(line[10:])
				result, err := client.Complete(prefix)
				if err != nil {
					fmt.Fprintf(os.Stderr, "Fehler: %v\n", err)
				} else {
					fmt.Println(result)
				}
				continue
			}
			if strings.HasPrefix(line, ":load ") {
				path := strings.TrimSpace(line[6:])
				result, err := client.LoadFile(path)
				if err != nil {
					fmt.Fprintf(os.Stderr, "Fehler: %v\n", err)
				} else {
					fmt.Println(result)
				}
				continue
			}
		}

		// Klammern zählen für Multiline
		multiline += line + "\n"
		openParens += countParens(line)

		if openParens <= 0 && strings.TrimSpace(multiline) != "" {
			// Ausdruck ist vollständig
			expr := strings.TrimSpace(multiline)
			result, err := client.Eval(expr)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Fehler: %v\n", err)
			} else {
				fmt.Println("=>", result)
			}
			multiline = ""
			openParens = 0
		}
	}
}

// countParens zählt offene Klammern
func countParens(s string) int {
	count := 0
	inString := false
	escape := false
	for _, ch := range s {
		if escape {
			escape = false
			continue
		}
		if ch == '\\' && inString {
			escape = true
			continue
		}
		if ch == '"' && !escape {
			inString = !inString
			continue
		}
		if !inString {
			if ch == '(' {
				count++
			} else if ch == ')' {
				count--
			}
		}
	}
	return count
}

func main() {
	// Flags definieren
	var (
		host     = flag.String("host", "localhost", "Server-Host")
		port     = flag.String("port", "4321", "Server-Port")
		evalFlag = flag.String("eval", "", "Expression auswerten")
		replFlag = flag.Bool("repl", false, "Interaktiver REPL-Modus")
		compFlag = flag.String("complete", "", "Autocomplete für Prefix")
		loadFlag = flag.String("load", "", "Datei laden")
		pingFlag = flag.Bool("ping", false, "Server-Ping")
	)
	flag.Parse()

	// Umgebungsvariablen prüfen
	if envHost := os.Getenv("GOLISP_HOST"); envHost != "" {
		*host = envHost
	}
	if envPort := os.Getenv("GOLISP_PORT"); envPort != "" {
		*port = envPort
	}

	// Client erstellen und verbinden
	client := NewClient(*host, *port)
	if err := client.Connect(); err != nil {
		fmt.Fprintf(os.Stderr, "Fehler: %v\n", err)
		os.Exit(1)
	}
	defer client.Close()

	// Ping testen
	if err := client.Ping(); err != nil {
		fmt.Fprintf(os.Stderr, "Server nicht erreichbar: %v\n", err)
		os.Exit(1)
	}

	// Befehl ausführen
	switch {
	case *pingFlag:
		fmt.Println("Server ist erreichbar: pong")

	case *evalFlag != "":
		result, err := client.Eval(*evalFlag)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Fehler: %v\n", err)
			os.Exit(1)
		}
		fmt.Println(result)

	case *compFlag != "":
		result, err := client.Complete(*compFlag)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Fehler: %v\n", err)
			os.Exit(1)
		}
		fmt.Println(result)

	case *loadFlag != "":
		result, err := client.LoadFile(*loadFlag)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Fehler: %v\n", err)
			os.Exit(1)
		}
		fmt.Println(result)

	case *replFlag:
		runREPL(client)

	default:
		// Default: einfacher Eval-Modus für "golisp-client 'expr'"
		if flag.NArg() > 0 {
			expr := flag.Arg(0)
			result, err := client.Eval(expr)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Fehler: %v\n", err)
				os.Exit(1)
			}
			fmt.Println(result)
		} else {
			// Keine Argumente → REPL
			runREPL(client)
		}
	}
}
