#lang racket/base

(require (submod "lobby-server/lobby-server.rkt" untyped))
(require (prefix-in rikken: "../games/rikken/server.rkt"))

(start-lobby-server #:games (GAMES ["rikken" rikken:SERF]))
(let loop ()
  (printf "type exit to quit:\n")
  (unless (equal? (read) 'exit) (loop)))
