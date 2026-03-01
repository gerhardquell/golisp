//**********************************************************************
//  cmd/golispd/main.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260301
//**********************************************************************
// GoLisp Server (Daemon) - SWANK-ähnlicher TCP-Server
// Bietet S-Expression-RPC für IDE-Integration
//**********************************************************************

package main

import (
	"flag"
	"fmt"
	"os"
	"os/signal"
	"syscall"

	"golisp/lib/swank"
)

func main() {
	// Flags parsen
	var (
		host = flag.String("host", "localhost", "Host für den Server (default: localhost)")
		port = flag.String("port", "4321", "Port für den Server (default: 4321)")
	)
	flag.Parse()

	// Umgebungsvariablen prüfen (haben Vorrang vor Flags)
	if envHost := os.Getenv("GOLISP_HOST"); envHost != "" {
		*host = envHost
	}
	if envPort := os.Getenv("GOLISP_PORT"); envPort != "" {
		*port = envPort
	}

	// Server erstellen
	server := swank.NewServer(*host, *port)

	// Signal-Handler für graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		<-sigChan
		fmt.Fprintln(os.Stderr, "\nServer wird beendet...")
		server.Stop()
	}()

	// Server starten (blockiert)
	if err := server.Start(); err != nil {
		fmt.Fprintf(os.Stderr, "Fehler: %v\n", err)
		os.Exit(1)
	}
}
