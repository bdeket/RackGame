#lang racket/base

(require (submod "lobby-server/lobby-server.rkt" untyped))
(require (prefix-in rikken: "../games/rikken/server.rkt")
         (prefix-in go: "go/server.rkt"))

(start-lobby-server #:games (GAMES ["rikken" rikken:SERF]
                                   ["go" go:SERF]))
(let loop ()
  (printf "type exit to quit:\n")
  (unless (equal? (read) 'exit) (loop)))
