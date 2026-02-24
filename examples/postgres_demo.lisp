; PostgreSQL Demo für GoLisp
;
; Voraussetzung: Laufender PostgreSQL-Server mit:
;   CREATE DATABASE golisp_test;
;   CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT, email TEXT);
;
; Verbindungs-String-Format:
;   "host=localhost port=5438 user=postgres password=secret dbname=golisp_test sslmode=disable"

; Verbindung herstellen
(define conn (pg-connect "host=localhost port=5438 user=postgres password=secret dbname=golisp_test sslmode=disable"))
(println "Connected to PostgreSQL")

; Tabelle erstellen (falls nicht vorhanden)
(pg-exec conn "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT, email TEXT)")
(println "Table created/verified")

; Daten einfügen
(define inserted (pg-exec conn "INSERT INTO users (name, email) VALUES ($1, $2)" "Alice" "alice@example.com"))
(println "Inserted rows: " inserted)

(define inserted2 (pg-exec conn "INSERT INTO users (name, email) VALUES ($1, $2)" "Bob" "bob@example.com"))
(println "Inserted rows: " inserted2)

; Daten abfragen
(println "\nAll users:")
(define users (pg-query conn "SELECT * FROM users"))
(println users)

; Abfrage mit Parameter
(println "\nUser with id=1:")
(define user1 (pg-query conn "SELECT * FROM users WHERE id = $1" 1))
(println user1)

; Verbindung schließen
(pg-close conn)
(println "\nConnection closed")
