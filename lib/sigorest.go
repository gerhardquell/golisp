//**********************************************************************
//  lib/sigorest.go
//  Autor    : Gerhard Quell - gquell@skequell.de
//  CoAutor  : claude sonnet 4.6
//  Copyright: 2026 Gerhard Quell - SKEQuell
//  Erstellt : 20260223
//**********************************************************************

package lib

import (
  "bytes"
  "encoding/json"
  "fmt"
  "io"
  "net/http"
  "strings"
  "sync"
  "time"
)

var (
  sigoHost    = "http://127.0.0.1:9080"
  sigoTimeout = 60 * time.Second
  // Rate-Limiting: max 1 Request pro 2 Sekunden pro Model
  sigoRateLimiter = time.Tick(2 * time.Second)
  // Circuit-Breaker Schutz
  sigoLastCall    time.Time
  sigoCallMutex   sync.Mutex
)

// RegisterSigo fügt (sigo prompt model session-id) in die Umgebung ein
func RegisterSigo(env *Env) {
  env.Set("sigo",        makeFn(fnSigo))
  env.Set("sigo-models", makeFn(fnSigoModels))
  env.Set("sigo-host",   makeFn(fnSigoHost))
}

// fnSigo: (sigo "prompt")
//         (sigo "prompt" "model")
//         (sigo "prompt" "model" "session-id")
//         (sigo "prompt" "model" "session-id" "host")
func fnSigo(args []*Cell) (*Cell, error) {
  if len(args) < 1 { return nil, fmt.Errorf("sigo: mindestens 1 Argument") }

  prompt    := args[0].Val
  model     := "ollama-gemma3-4b"
  sessionID := ""
  host      := sigoHost

  if len(args) >= 2 { model = args[1].Val }
  if len(args) >= 3 { sessionID = args[2].Val }
  if len(args) >= 4 { host = strings.TrimRight(args[3].Val, "/") }

  // Rate-Limiting: Warte auf Token im Ticker
  <-sigoRateLimiter

  // Circuit-Breaker Schutz: mindestens 500ms zwischen Calls
  sigoCallMutex.Lock()
  sinceLast := time.Since(sigoLastCall)
  if sinceLast < 500*time.Millisecond {
    time.Sleep(500*time.Millisecond - sinceLast)
  }
  sigoLastCall = time.Now()
  sigoCallMutex.Unlock()

  result, err := sigoCallToHost(prompt, model, sessionID, host)
  if err != nil { return nil, err }
  return MakeStr(result), nil
}

// fnSigoModels: (sigo-models) → Liste der verfügbaren Modelle
func fnSigoModels(args []*Cell) (*Cell, error) {
  resp, err := http.Get(sigoHost + "/v1/models")
  if err != nil { return nil, fmt.Errorf("sigo-models: %v", err) }
  defer resp.Body.Close()

  body, _ := io.ReadAll(resp.Body)

  var data struct {
    Data []struct{ ID string `json:"id"` } `json:"data"`
  }
  if err := json.Unmarshal(body, &data); err != nil {
    return nil, fmt.Errorf("sigo-models parse: %v", err)
  }

  result := MakeNil()
  for i := len(data.Data) - 1; i >= 0; i-- {
    result = Cons(MakeStr(data.Data[i].ID), result)
  }
  return result, nil
}

// fnSigoHost: (sigo-host "http://192.168.1.10:9080") → Host ändern
func fnSigoHost(args []*Cell) (*Cell, error) {
  if len(args) < 1 { return MakeStr(sigoHost), nil }
  sigoHost = strings.TrimRight(args[0].Val, "/")
  return MakeStr(sigoHost), nil
}

// sigoCall sendet einen Chat-Request an sigoREST
func sigoCall(prompt, model, sessionID string) (string, error) {
  return sigoCallToHost(prompt, model, sessionID, sigoHost)
}

// sigoCallToHost sendet einen Chat-Request an einen bestimmten Host
func sigoCallToHost(prompt, model, sessionID, host string) (string, error) {
  reqBody := map[string]interface{}{
    "model": model,
    "messages": []map[string]string{
      {"role": "user", "content": prompt},
    },
  }
  if sessionID != "" {
    reqBody["session_id"] = sessionID
  }

  data, err := json.Marshal(reqBody)
  if err != nil { return "", fmt.Errorf("sigo marshal: %v", err) }

  client := &http.Client{Timeout: sigoTimeout}
  resp, err := client.Post(
    host+"/v1/chat/completions",
    "application/json",
    bytes.NewReader(data),
  )
  if err != nil { return "", fmt.Errorf("sigo connect: %v", err) }
  defer resp.Body.Close()

  body, _ := io.ReadAll(resp.Body)

  if resp.StatusCode != 200 {
    return "", fmt.Errorf("sigo HTTP %d: %s", resp.StatusCode, string(body))
  }

  var result struct {
    Choices []struct {
      Message struct {
        Content string `json:"content"`
      } `json:"message"`
    } `json:"choices"`
  }
  if err := json.Unmarshal(body, &result); err != nil {
    return "", fmt.Errorf("sigo parse: %v", err)
  }
  if len(result.Choices) == 0 {
    return "", fmt.Errorf("sigo: leere Antwort")
  }
  return result.Choices[0].Message.Content, nil
}
