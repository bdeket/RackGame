#lang typed/racket/base

(require "datastructs.rkt")

(provide (all-defined-out))

(define (mail-all [S : server][msg : Msg]) : (Listof Mail)
  (for/list ([(Pid P)(in-hash (server-players S))])
    (mail Pid msg)))

(define (mail-rooms [S : server][msg : Msg]) : (Listof Mail)
  (for/list ([(Pid P)(in-hash (server-players S))]
             #:when (equal? (location-type (player-location P)) 'room))
    (mail Pid msg)))

(define (mail-players [Pids : (Listof ID)][msg : Msg]) : (Listof Mail)
  (for/list ([Pid (in-list Pids)])
    (mail Pid msg)))

(define (rooms->msg [S : Server][Pid : ID])
  (define L (hash-ref (server-rooms S) lobby-rid))
  (define (mk [id : Room-ID][r : Room])
    (MSG [id id]
         [name (room-name r)]
         [players (map (λ ([n : ID]) (MSG [user n]
                                          [state (player-state (server-player S n))]))
                       (remove Pid (room-players r)))]))
  (MSG "rooms"
       [user Pid]
       [rooms (cons (mk lobby-rid L)
                    (for/list : (Listof MSGval)
                      ([([id : Room-ID][r : Room])(in-hash (hash-remove (server-rooms S) lobby-rid))])
                      (mk id r)))]))
