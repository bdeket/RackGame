#lang typed/racket/base

(require "../../server/util.rkt")

(provide (all-defined-out)
         (all-from-out "../../server/util.rkt"))

(module+ untyped
  (provide (struct-out player)
           (struct-out bord)
           (struct-out server)
           player:wait player:pass player:play player:check player:dead
           state:playing state:counting state:gameDone
           first-player first-Pid next-player next-Pid
           make-bord bord-get bord-add
           group-explore explore))

;*******************************************************************************
; Datastructures
;*******************************************************************************
(struct player ([name : ID]
                [state : Player-State]
                [points : Nonnegative-Integer]
                ))(define-type Player player)
(struct bord ([size : Nonnegative-Integer]
              [b : (Immutable-HashTable Exact-Number (Option ID))]
              ))(define-type Bord bord)
(struct server ([players : (Listof Player)]
                [bord : Bord]
                [bord-1 : Bord]
                [bord-c : Bord]
                [firstPlayer : ID]
                [state : Server-State]
                ))(define-type Server server)
(define *SIZE* 9)

(define-type Player-State (U "wait" "pass" "play" "check" "dead"))
(define player:wait : Player-State "wait")
(define player:pass : Player-State "pass")
(define player:play : Player-State "play")
(define player:check : Player-State "check")
(define player:dead : Player-State "dead")

(define-type Server-State (U "playing" "counting" "gameDone"))
(define state:playing : Server-State "playing")
(define state:counting : Server-State "counting")
(define state:gameDone : Server-State "gameDone")

(define (player-set-state [S : server][Pid : ID][state : Player-State]) : Server
  (struct-copy server S
               [players (for/list : (Listof Player)
                          ([P (in-list (server-players S))])
                          (if (equal? Pid (player-name P))
                              (struct-copy player P
                                           [state state])
                              P))]))
(define (player-add-points [S : server][Pid : ID][points : Nonnegative-Integer]) : Server
  (struct-copy server S
               [players (for/list : (Listof Player)
                          ([P (in-list (server-players S))])
                          (if (equal? Pid (player-name P))
                              (struct-copy player P
                                           [points (+ (player-points P) points)])
                              P))]))

(define (first-player [S : Server]) : Player
  (car (server-players S)))
(define (next-player [S : Server]) : Player
  (cadr (server-players S)))
(define (first-Pid [S : Server]) : ID
  (player-name (first-player S)))
(define (next-Pid [S : Server]) : ID
  (player-name (next-player S)))

;*******************************************************************************
; Bord
;*******************************************************************************
(define (make-bord [size : Nonnegative-Integer])
  (bord
   size
   (for*/hash : (Immutable-HashTable Exact-Number (Option ID))
     ([i (in-range size)]
      [j (in-range size)])
     (values (make-rectangular i j) #f))))

(define (bord-get [B : Bord][pos : Exact-Number])(hash-ref (bord-b B) pos))
(define (bord-add [B : Bord][pos : Exact-Number][id : ID]) : Bord
  (cond
    [(in-bord? B pos) (struct-update bord B [b (λ (H)(hash-set H pos id))])]
    [else (error "bord-add: not a valid position" pos)]))
(define (bord-remove [B : Bord][poss : (Listof Exact-Number)])
  (struct-update bord B
                 [b (λ (H)
                      (for/fold  ([H : (Immutable-HashTable Exact-Number (Option ID)) H])
                                 ([p (in-list poss)])
                        (hash-set H p #f)))]))
(define (in-bord? [B : Bord][pos : Exact-Number]) : Boolean
  (and (< -1 (real-part pos) (bord-size B))
       (< -1 (imag-part pos) (bord-size B))))
(define (adjacent-pos [B : Bord][pos : Exact-Number])
  (filter (λ ([p : Exact-Number]) (in-bord? B p))
          (list (+ pos 1)(- pos 1)(+ pos +i)(- pos +i))))
(define (bord-equal? [B1 : Bord][B2 : Bord]) : Boolean
  (and (equal? (bord-size B1)(bord-size B2))
       (equal? (bord-b B1)(bord-b B2))))

(define (free-pos [S : Server][pos : Exact-Number]) : Boolean
  (define B (server-bord S))
  (and (< -1 (real-part pos) (bord-size B))
       (< -1 (imag-part pos) (bord-size B))
       (equal? (hash-ref (bord-b B) pos #f) #f)))
(define (get-pos [S : Server][pos : Exact-Number]) : (Option ID) (bord-get (server-bord S) pos))

(define (group-explore [B : Bord][pos : Exact-Number]) : (Values (Listof Exact-Number)(Listof Exact-Number))
  (define id (bord-get B pos))
  (let loop ([toTest : (Listof Exact-Number) (if id (adjacent-pos B pos) '())]
             [tested : (Listof Exact-Number) (if id (list pos) '())]
             [group  : (Listof Exact-Number) (list pos)]
             [free   : (Listof Exact-Number) '()])
    (cond
      [(null? toTest) (values group free)]
      [else
       (define p1 (car toTest))
       (define id1 (bord-get B p1))
       (define tested+ (cons p1 tested))
       (define toTest+ (cdr toTest))
       (cond
         [(equal? id1 #f)(loop toTest+ tested+ group (cons p1 free))]
         [(equal? id1 id)(loop (append toTest+
                                       (remove* (append toTest tested+) (adjacent-pos B p1)))
                               tested+
                               (cons p1 group)
                               free)]
         [else (loop toTest+ tested+ group free)])])))

(define (explore [B : Bord][pos : Exact-Number]) : (Values (Listof Exact-Number)(Immutable-HashTable (Option ID) (Listof Exact-Number)))
  (define id (bord-get B pos))
  (let loop ([toTest : (Listof Exact-Number) (if id (adjacent-pos B pos) '())]
             [tested : (Listof Exact-Number) (if id (list pos) '())]
             [group  : (Listof Exact-Number) (list pos)]
             [neighbours : (Immutable-HashTable (Option ID) (Listof Exact-Number)) #hash((#f . ()))])
    (cond
      [(null? toTest) (values group neighbours)]
      [else
       (define p1 (car toTest))
       (define id1 (bord-get B p1))
       (define tested+ (cons p1 tested))
       (define toTest+ (cdr toTest))
       (cond
         [(equal? id1 id)
          (loop (append toTest+
                        (remove* (append toTest tested+) (adjacent-pos B p1)))
                tested+
                (cons p1 group)
                neighbours)]
         [else
          (loop toTest+ tested+ group
                (hash-update neighbours id1
                             (λ ([L : (Listof Exact-Number)])
                               (cons p1 L))
                             (λ () '())))])])))

