#lang typed/racket/base

(require "../util.rkt")

(provide (all-defined-out)
         (all-from-out "../util.rkt"))

(define-logger LobServ)

;*******************************************************************************
; Room
;*******************************************************************************
(struct room ([name : String][game : Game-ID][state : Room-State][players : (Listof ID)]))(define-type Room room)
(define-new-subtype Room-ID [room-id String])
(define-new-subtype Game-ID [game-id String])
(define-type Room-State (U "waiting" "ready"))
(define room:waiting : Room-State "waiting")
(define room:ready : Room-State "ready")
(define (new-room [name : String][game : Game-ID])(room name game room:waiting '()))

(define (server-room/f [S : server][Rid : Room-ID])(hash-ref (server-rooms S) Rid #f))
(define (server-room [S : server][Rid : Room-ID])(or (server-room/f S Rid) (error "room not defined" Rid)))
(structhashstruct-updater room-update server rooms room)
(structhashstruct-copier room-set server rooms room)

(define (server-room-ready? [S : server][Rid : Room-ID])
  (define R (server-room S Rid))
  (and (andmap (λ ([Pid : ID]) (equal? (server-player-state S Pid) player:ready)) (room-players R))
       (let ([G (server-game S (room-game R))])
         (<= (game-min G) (length (room-players R)) (game-max G)))))
(define (server-room-players [S : server][Rid : Room-ID])(room-players (server-room S Rid)))
(define (server-room-state [S : server][Rid : Room-ID])(room-state (server-room S Rid)))
;*******************************************************************************
; Bout
;*******************************************************************************
(struct bout ([name : String]
              [players : (Listof ID)]
              [workers : (Listof ID)]
              [started : Boolean]
              [time : Flonum]
              [tick-rate : Positive-Real]
              [world : Path-String]
              [start :      (-> Worker-Fct        (Values (Listof Mail)(Listof ID)))]
              [disconnect : (-> ID Worker-Fct     (Values (Listof Mail)(Listof ID)))]
              [message :    (-> ID Msg Worker-Fct (Values (Listof Mail)(Listof ID)))]
              [tick :       (-> Worker-Fct        (Values (Listof Mail)(Listof ID)))]))(define-type Bout bout)
(define-new-subtype Bout-ID [bout-id String])
(define-type Location (U (Pair 'room Room-ID)(Pair 'bout Bout-ID)))
(define (room-location [id : Room-ID])(cons 'room id))
(define (bout-location [id : Bout-ID])(cons 'bout id))
(define (location-type [L : Location])(car L))
(define (location-id [L : Location])(cdr L))

(define (server-bout/f [S : server][Bid : Bout-ID])(hash-ref (server-bouts S) Bid #f))
(define (server-bout [S : server][Bid : Bout-ID])(or (server-bout/f S Bid) (error "bout not defined" Bid)))
(structhashstruct-updater bout-update server bouts bout)
(structhashstruct-copier bout-set server bouts bout)

;*******************************************************************************
; Game
;*******************************************************************************
(struct game ([name : String]
              [world : Path-String]
              [min : Positive-Integer]
              [max : Positive-Integer]
              [tick-rate : Positive-Real]
              [toBout : (-> String (Listof ID) bout)]))(define-type Game game)
(define (gameserver->game [G : Gameserver]) : Game
  (define tr (gameserver-tick-rate G))
  (define world (gameserver-world G))
  (game (gameserver-name G)
        world
        (gameserver-min G)
        (gameserver-max G)
        tr
        (λ ([name : String][Pids : (Listof ID)])
          (define GI ((gameserver-toInstance G)))
          (bout name
                Pids
                '()
                #f
                (+ (current-inexact-milliseconds) tr)
                tr
                world
                (λ ([CW : Worker-Fct])((gameinstance-start GI) Pids CW))
                (gameinstance-disconnect GI)
                (gameinstance-message GI)
                (gameinstance-tick GI)))))

(define (server-game/f [S : server][Gid : Game-ID]) : (Option game) (hash-ref (server-games S) Gid #f))
(define (server-game [S : server][Gid : Game-ID]) : game (or (server-game/f S Gid) (error "game not defined" Gid)))

;*lobby*************************************************************************
(define lobby-game (game "Lobby" "lobby.js" 1 (expt 10 30) +inf.0 (λ (name Pids) (error "Lobby can't be played"))))
(define lobby-rid (room-id "lobby"))
(define lobby-gid (game-id "lobby"))

;*******************************************************************************
; Player
;*******************************************************************************
(struct player ([name : ID][state : Player-State][location : Location]))(define-type Player player)
(define-type Player-State (U "waiting" "canjoin" "ready" "playing" "loadingGame"))
(define player:waiting : Player-State "waiting")
(define player:canjoin : Player-State "canjoin")
(define player:ready : Player-State "ready")
(define player:playing : Player-State "playing")
(define player:loadingGame : Player-State "loadingGame")
(define (new-player [name : ID][loc : Location])(player name player:waiting loc))

(define (server-player/f [S : server][Pid : ID])(hash-ref (server-players S) Pid #f))
(define (server-player [S : server][Pid : ID])(or (server-player/f S Pid) (error "player not defined" Pid)))
(structhashstruct-updater player-update server players player)
(structhashstruct-copier player-set server players player)

(define (server-player-location [S : server][Pid : ID]) (player-location (server-player S Pid)))
(define (server-player-location-id [S : server][Pid : ID]) (location-id (server-player-location S Pid)))
(define (server-player-state [S : server][Pid : ID]) : Player-State (player-state (server-player S Pid)))

;*******************************************************************************
; server
;*******************************************************************************
(struct server ([players : (Immutable-HashTable ID Player)]
                [workers : (Immutable-HashTable ID Bout-ID)]
                [rooms : (Immutable-HashTable Room-ID Room)]
                [games : (Immutable-HashTable Game-ID Game)]
                [bouts : (Immutable-HashTable Bout-ID Bout)]
                [create-worker : Worker-Fct]))(define-type Server server)

(define (new-server [games : (Immutable-HashTable Game-ID game)]
                    [create-worker : Worker-Fct])
  (server #hash()
          #hash()
          (make-immutable-hash (list (cons lobby-rid (new-room "Lobby" lobby-gid))))
          (hash-set games (game-id "lobby") lobby-game)
          #hash()
          create-worker))

(define (server-worker/f [S : server][Wid : ID])(hash-ref (server-workers S) Wid #f))
(define (server-worker [S : server][Wid : ID])(or (server-worker/f S Wid) (error "worker not defined" Wid)))

(define (add-worker [S : server][Bid : Bout-ID][Wid : ID])
  (struct-update server S
                 [workers (λ (H) (hash-set H Wid Bid))]
                 [bouts (λ (H)
                          (hash-update H Bid
                                       (λ (B)
                                         (struct-copy bout B [workers (cons Wid (bout-workers B))]))))]))

;*******************************************************************************
; Helper...
;*******************************************************************************
(define (first-free-name/id [id : String][in-group : (-> String Any)][name : String id]) : (Values String String)
  (define (mkid [i : Integer]) : String (format "~a~a" id i))
  (define i
    (let loop : Integer ([i 1])
      (define id (mkid i))
      (cond
        [(equal? #f (in-group id)) i]
        [else (loop (+ i 1))])))
  (values (format "~a ~a" name i) (mkid i)))
