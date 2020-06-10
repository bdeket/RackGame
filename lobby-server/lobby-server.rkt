#lang typed/racket/base

(require racket/match
         racket/format)
(require "../server/connection-server.rkt")
(require "datastructs.rkt"
         "mail.rkt")

(provide start-lobby-server GAMES)
(module* untyped racket/base
  (require (only-in (submod "..") start-lobby-server)
           (only-in "datastructs.rkt" game-id gameserver->game))
  (define-syntax-rule (GAMES [ID GS] ...) (make-immutable-hash (list (cons (game-id ID) (gameserver->game GS)) ...)))
  (provide start-lobby-server GAMES))

(define waiting-for-players (gensym 'waiting))

(define-syntax-rule (define-with-worker (S-id M-id D-id cw-id) S-start Bid fct)
  (begin
    (define S-id S-start)
    (define CW (server-create-worker S-start))
    (define (cw-id [w : Worker] #:name [nm : String "worker"])
      (define name (CW w #:name nm))
      (set! S-id (add-worker S-id Bid name))
      name)
    (define-values (M-id D-id)
      (with-handlers ([exn:fail? (λ ([e : exn])
                                   (log-LobServ-error "error  # in bout ~a\n\033[031;1m~a\033[0m\n~a"
                                                      Bid (exn-message e)
                                                      (apply
                                                       string-append
                                                       (for/list : (Listof String)
                                                         ([s (in-list (continuation-mark-set->context (exn-continuation-marks e)))])
                                                         (format "  ~a\n" s))))
                                   (values '() '()))])
        fct))))

;*******************************************************************************
; The server
;*******************************************************************************
(define-syntax-rule (GAMES [ID GS] ...) (make-immutable-hash (list (cons (game-id ID) (gameserver->game GS)) ...)))
(define (start-lobby-server #:games [games : (Immutable-HashTable Game-ID Game)
                                           (make-immutable-hash (list (cons (game-id "dummy") (gameserver->game dummyserver))))]
                            #:port [port : Positive-Integer PORT])
  ((inst start-connection-server Server)
   (λ ([create-worker : Worker-Fct]) : (Bundle Server)
     (make-bundle (new-server games create-worker)))
   #:on-connection sconnection
   #:on-disconnect sdisconnect
   #:on-message smessage
   #:on-tick stick
   #:tick-rate (max 1 (* 500 (apply min 120 (hash-map games (λ ([id : String][g : Game]) : Positive-Real (game-tick-rate g))))))
   ;game-tick-rate is in seconds
   ;*500 to make the server tick twice as fast
   #:port port))

;*******************************************************************************
; new connections
;*******************************************************************************
(define (sconnection [S : Server][Pid : ID]) : (Bundle Server)
  (define P (new-player Pid (room-location lobby-rid)))
  (define S0 (struct-update server S [players (λ (H)(hash-set H Pid P))]))
  (define S1 (room-update S0 lobby-rid [players (λ (Ps)`(,@Ps ,Pid))]))
  (lobby-lobbyReady S1 Pid lobby-rid))

;*******************************************************************************
; Lost connections
;*******************************************************************************
(define (sdisconnect [S : Server][Pid : ID]) : (Bundle Server)
  (define loc (player-location (hash-ref (server-players S) Pid)))
  (define S0 (struct-update server S [players (λ (H) (hash-remove H Pid))]))
  (case (location-type loc)
    ; in the lobby
    [(room)
     (define BS (make-bundle S0))
     (bs-room-remove-player BS (location-id loc) Pid)]
    ; in a game
    [(bout)
     (define Bid (location-id loc))
     (define B (server-bout S0 Bid))
     (cond
       [(bout-started B)
        (define S1 (bout-update S0 Bid [players (λ (L)(remove Pid L))]))
        (define-with-worker (S2 M2 D2 cw) S1 Bid ((bout-disconnect B) Pid cw))
        (make-bundle S2 M2 D2)]
       [else (bout-abandonGame S0 B)])]
    [else (log-LobServ-error "error  # unknown location ~a" loc)]))

;*******************************************************************************
; Messages
;*******************************************************************************
(define (smessage [S : Server][Pid : ID][M : Msg]) : (Bundle Server)
  (define loc (if (server-player/f S Pid)
                  (server-player-location S Pid)
                  (bout-location (server-worker S Pid))))
  (case (location-type loc)
    [(room) (handle-lobby-message S Pid M (location-id loc))]
    [(bout) (handle-bout-message S Pid M (location-id loc))]
    [else (log-LobServ-error "error  # unknown location: ~a" loc)]))

;*******************************************************************************
; Messages for the Lobby
;*******************************************************************************
(define (handle-lobby-message [S : Server][Pid : ID][M : Msg][Rid : Room-ID]) : (Bundle Server)
  (match-msg
   M
   [(msg: "lobbyReady") (lobby-lobbyReady S Pid Rid)]
   [(msg: "joinroom" [room ($$ newRid (and (string? newRid)
                                           (server-room/f S (room-id newRid))))])
    (lobby-joinRoom S Pid Rid (room-id newRid))]
   [(msg: "create")(lobby-create S Pid Rid)]
   [(msg: "createRoom" [game ($$ Gid (and (string? Gid)
                                          (server-game/f S (game-id Gid))))])
    (lobby-createRoom S Pid Rid (game-id Gid))]
   [(msg: "ready") (lobby-ready S Pid Rid)]
   [(msg: "wait")  (lobby-wait S Pid Rid)]
   [(msg: "start") (lobby-start S Pid Rid)]
   [else
    (log-LobServ-warning "warning# ~a send unknown message: ~a" Pid M)
    (make-bundle S)]))

(define (lobby-lobbyReady [S : Server][Pid : ID][Rid : Room-ID]) : (Bundle Server)
  (make-bundle S (list* (mail Pid (rooms->msg S Pid))
                        (mail-rooms S (MSG "roomIn" [user Pid][room "lobby"])))))

(define (lobby-joinRoom [S : Server][Pid : ID][Rid : Room-ID][newRid : Room-ID]) : (Bundle Server)
  (define L (room-location newRid))
  (define P (server-player S Pid))
  (cond
    ;player already in this room
    [(equal? Rid newRid) (make-bundle S)]
    [else
     (define newR (server-room S newRid))
     (define G (server-game S (room-game newR)))
     (cond
       [(<= (game-max G) (length (room-players newR))) (make-bundle S)]
       [else
        (define P+ (struct-copy player P [state player:waiting][location L]))
        (define B+
          (make-bundle (player-set S Pid [state player:waiting][location L])
                       (mail-rooms S (MSG "playerState"
                                          [user Pid]
                                          [state (if (equal? newRid lobby-rid)
                                                     player:waiting
                                                     player:canjoin)]))))
        (bs-room-add-player
         (bs-room-remove-player B+ Rid Pid)
         newRid Pid)])]))

(define (lobby-create [S : Server][Pid : ID][Rid : Room-ID]) : (Bundle Server)
  (make-bundle S
               (list (mail Pid (MSG "gameList"
                                    [games (for/list : (Listof MSGval)
                                             ([(Gid G)(in-hash (hash-remove (server-games S) lobby-gid))])
                                             (MSG [id Gid][name (game-name G)]))])))))

(define (lobby-createRoom [S : Server][Pid : ID][Rid : Room-ID][Gid : Game-ID]) : (Bundle Server)
  (define G (server-game S Gid))
  (define-values (name Rid)
    (let-values ([([name : String][Rid : String])
                  (first-free-name/id Gid (λ ([x : String])(server-room/f S (room-id x))) (game-name G))])
      (values name (room-id Rid))))
  (define name+ (format "~a [~a - ~a]P" name (game-min G)(game-max G)))
  (make-bundle (struct-update server S
                              [rooms (λ (H) (hash-set H Rid (new-room name+ Gid)))])
               (mail-rooms S (MSG "newRoom" [id Rid][name name+]))))

(define (lobby-ready [S : Server][Pid : ID][Rid : Room-ID]) : (Bundle Server)
  (cond
    [(equal? Rid "lobby") (make-bundle S)]
    [else
     (define S+ (player-set S Pid [state player:ready]))
     (define statemail (mail-rooms S (MSG "playerState" [user Pid][state player:ready])))
     (cond
       [(server-room-ready? S+ Rid)
        (make-bundle
         (room-set S+ Rid [state room:ready])
         (append
          statemail
          (mail-rooms S (MSG "roomState" [room Rid][state room:ready]))))]
       [else
        (make-bundle S+ statemail)])]))

(define (lobby-wait [S : Server][Pid : ID][Rid : Room-ID]) : (Bundle Server)
  (define st (if (equal? Rid "lobby") player:waiting player:canjoin))
  (make-bundle (player-set S Pid [state st])
               (mail-rooms S (MSG "playerState" [user Pid][state st]))))

(define (lobby-start [S : Server][Pid : ID][Rid : Room-ID]) : (Bundle Server)
  (cond
    [(server-room-ready? S Rid)
     (define Gid (room-game (server-room S Rid)))
     (define-values (name Bid)
       (let-values ([([name : String][Bid : String])
                     (first-free-name/id Gid (λ ([x : String])(server-bout/f S (bout-id x))))])
         (values name (bout-id Bid))))
     (define Ps (room-players (server-room S Rid)))
     (define GS (server-game S Gid))
     (define B ((game-toBout GS) name Ps))
     (define S+ (struct-update server S
                               [rooms (λ (H)(hash-remove H Rid))]
                               [bouts (λ (H)(hash-set H Bid B))]
                               [players
                                (λ (H)
                                  (for/fold : (Immutable-HashTable ID Player)
                                    ([H : (Immutable-HashTable ID Player) H])
                                    ([pid (in-list Ps)])
                                    (hash-update
                                     H pid
                                     (λ ([P : Player])
                                       (struct-copy player P
                                                    [state player:loadingGame]
                                                    [location (bout-location Bid)])))))]))
     (make-bundle S+
                  (append
                   (apply
                    append
                    (for/list : (Listof (Listof Mail))
                      ([Pid (in-list Ps)])
                      (mail-rooms S+ (MSG "roomOut" [user Pid][room Rid]))))
                   (mail-rooms S+ (MSG "deleteRoom" [room Rid]))
                   (mail-players Ps (MSG "startGame" [world (~a (game-world GS))]))))]
    [else (make-bundle S)]))
;*******************************************************************************
; Messages for a Bout
;*******************************************************************************
(define (handle-bout-message [S : Server][Pid : ID][M : Msg][Bid : Bout-ID]) : (Bundle Server)
  (define B (server-bout S Bid))
  (case (msg-tag M)
    [("leaveGame") (bout-leaveGame S Pid Bid B)]
    [("gameReadyToStart") (bout-gameReadyToStart S Pid Bid B)]
    [else (bout-pass-to-game S Pid M Bid B)]))

(define (bout-leaveGame [S : Server][Pid : ID][Bid : Bout-ID][B : Bout]) : (Bundle Server)
  (cond
    [(bout-started B)
     (define-with-worker (S0 M1 D1 cw) S Bid ((bout-disconnect B) Pid cw))
     ((inst bundle-update Server)
      (sconnection S0 Pid)
      #:u-state (λ (S) (bout-update S Bid [players (λ (L) (remove Pid L))]))
      #:u-mails (λ (S [M : (Listof Mail)])
                  `(,@M1
                    ,(mail Pid (MSG "leaveGame" [world (~a (bout-world B))]))
                    ,@M))
      #:a-drops D1)]
    [else
     (bout-abandonGame S B)]))

(define (bout-abandonGame[S : Server][B : Bout]) : (Bundle Server)
  (log-LobServ-info "info   # abandoning bout ~a" (bout-name B))
  (for/fold ([BS (make-bundle S)])
            ([Pid (in-list (bout-players B))])
    (defbundle (S M D) BS)
    ((inst bundle-update Server)
     (sconnection S Pid)
     #:u-mails (λ (S [M : (Listof Mail)]) (cons (mail Pid (MSG "leaveGame" [world (~a (bout-world B))])) M))
     #:a-drops D)))

(define (bout-gameReadyToStart [S : Server][Pid : ID][Bid : Bout-ID][B : Bout]) : (Bundle Server)
  (cond
    [(equal? (server-player-state S Pid) player:loadingGame)
     (define S0 (player-set S Pid [state player:playing]))
     (cond
       [(for/and : Boolean ([pid (in-list (bout-players B))])
          (equal? (server-player-state S0 pid) player:playing))
        (define-with-worker (S1 M1 D1 cw) S0 Bid ((bout-start B) cw))
        (make-bundle
         (bout-set S1 Bid [started #t])
         M1 D1)]
       [else (make-bundle S0)])]
    [else (make-bundle S)]))

(define (bout-pass-to-game [S : Server][Pid : ID][M : Msg][Bid : Bout-ID][B : Bout]) : (Bundle Server)
  (cond
    [(not (bout-started B))
     (log-LobServ-info "info   # waiting for players to load game - ignoring message: ~a" M)
     (make-bundle S)]
    [else
     (define-with-worker (S1 M1 D1 cw) S Bid ((bout-message B) Pid M cw))
     (make-bundle S1 M1 D1)]))

;*******************************************************************************
; Server tick's
;*******************************************************************************
(define (stick [S : Server]) : (Bundle Server)
  (for/fold : (Bundle server)
    ([BS (make-bundle S)])
    ([(Bid B) (in-hash (server-bouts S))])
    (cond
      ;if there are no players in the bout, delete it
      ;and remove any workers it might have
      [(null? (bout-players B))
       ((inst bundle-update server)
        BS
        #:u-state (λ (S)
                    (struct-update server S
                                   [bouts (λ (H) (hash-remove H Bid))]))
        #:a-drops (bout-workers B))]
      ;else check if enough time has passed to do a tick action
      [(and (bout-started B)
            (< (bout-time B)(current-inexact-milliseconds)))
       (defbundle (S0) BS)
       (define S1 (bout-update S Bid [time (λ (t)(+ t (* 1000 (bout-tick-rate B))))]))
       (define-with-worker (S2 M D cw) S1 Bid ((bout-tick B) cw))
       ((inst bundle-update server) BS #:s-state S2 #:a-mails M #:a-drops D)]
      [else BS])))

;*******************************************************************************
; Helper functions
;*******************************************************************************
(define (bs-room-remove-player [BS : (Bundle server)][Rid : Room-ID][Pid : ID]) : (Bundle server)
  (define S (bundle-state BS))
  (define M (append (bundle-mails BS)
                    (mail-rooms S (MSG "roomOut" [user Pid][rom Rid]))))
  (define D (bundle-drops BS))
  (define S+
    (room-update S Rid [players (λ (L) (remove Pid L))]))
  (if (server-room-ready? S+ Rid)
      (if (equal? (server-room-state S Rid) room:ready)
          (bundle S+ M D)
          (bundle (room-set S+ Rid [state room:ready])
                  (append M (mail-rooms S+ (MSG "roomState" [room Rid][state room:ready])))
                  D))
      (if (equal? (server-room-state S Rid) room:ready)
          (bundle (room-set S+ Rid [state room:waiting])
                  (append M (mail-rooms S+ (MSG "roomState" [room Rid][state room:waiting])))
                  D)
          (bundle S+ M D))))

(define (bs-room-add-player [BS : (Bundle server)][Rid : Room-ID][Pid : ID]) : (Bundle server)
  (define S (if (bundle? BS) (bundle-state BS) BS))
  (define M (append (if (bundle? BS) (bundle-mails BS) '())
                    (mail-rooms S (MSG "roomIn" [user Pid][room Rid]))
                    (mail-rooms S (MSG "roomState" [room Rid][state room:waiting]))))
  (define D (if (bundle? BS) (bundle-drops BS) '()))
  (define S+
    (room-update S Rid
                 [players (λ (L) (cons Pid L))]
                 [state (λ (st) room:waiting)]))
  (bundle S+ M D))


(module+ test (start-lobby-server))
