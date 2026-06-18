//**********************************************************************
//  lib/swank/framing.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260618
//**********************************************************************
// SWANK length-prefixed UTF-8 framing.
//**********************************************************************

package swank

import (
  "bufio"
  "fmt"
  "io"
  "strconv"

  "golisp/lib"
)

// readFrame reads one SWANK length-prefixed S-expression.
func readFrame(r io.Reader) (*lib.Cell, error) {
  br := bufio.NewReader(r)
  line, err := br.ReadString('\n')
  if err != nil {
    return nil, fmt.Errorf("readFrame: %w", err)
  }
  line = line[:len(line)-1] // drop '\n'
  n, err := strconv.ParseInt(line, 16, 32)
  if err != nil {
    return nil, fmt.Errorf("readFrame: invalid length %q: %w", line, err)
  }
  payload := make([]byte, n)
  if _, err := io.ReadFull(br, payload); err != nil {
    return nil, fmt.Errorf("readFrame: short read: %w", err)
  }
  cell, err := lib.Read(string(payload))
  if err != nil {
    return nil, fmt.Errorf("readFrame: parse: %w", err)
  }
  return cell, nil
}

// writeFrame writes one SWANK length-prefixed S-expression.
func writeFrame(w io.Writer, cell *lib.Cell) error {
  payload := cell.String()
  frame := fmt.Sprintf("%06x\n%s", len(payload), payload)
  _, err := io.WriteString(w, frame)
  if err != nil {
    return fmt.Errorf("writeFrame: %w", err)
  }
  return nil
}
