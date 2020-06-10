#lang typed/racket/base

(require threading
         racket/match)
(require "datastructs.rkt")

(provide SERF)

;*******************************************************************************
;new Server
;*******************************************************************************
(define empty-server (server (list (player (id "") player:dead 0)
                                   (player (id "") player:dead 0))
                             (make-bord *SIZE*)
                             (make-bord *SIZE*)
                             (make-bord *SIZE*)
                             (id "")
                             state:gameDone))
(define (make-server [S0 : Server]
                     [Pids : (Listof ID)]
                     [create-worker : Worker-Fct]) : (Bundle Server)
  (make-server+ Pids))
(define (make-server+ [Pids : (Listof ID)])
  (define S1
    (struct-copy server empty-server
                 [players (list (player (car Pids) player:play 0)
                                (player (cadr Pids) player:wait 0))]
                 [firstPlayer (car Pids)]
                 [state state:playing]))
  (make-bundle S1
               (append (mail:report S1)
                       (mail:playerState S1))))


;*******************************************************************************
;lost connections
;*******************************************************************************
(define (disconnect [S0 : Server][Pid : ID][create-worker : Worker-Fct]) : (Bundle Server)
  (define S1 (player-set-state S0 Pid player:dead))
  (cond
    [(equal? (server-state S1) state:gameDone)
     (make-bundle S1 (mail:playerState S1))]
    [else
     (make-bundle
      (struct-copy server S1 [state state:gameDone])
      (append (mail:gameDone S1)
              (mail:playerState S1)))]))

;*******************************************************************************
;message handling
;*******************************************************************************
(define (message [S : Server][Pid : ID][M : Msg][create-worker : Worker-Fct]) : (Bundle Server)
  (match-msg M
   [(msg: "place" [x x][y y])
    (cond
      [(and (exact-integer? x)(exact-integer? y))
       (define p (make-rectangular x y))
       (match (server-state S)
         [(== state:playing) (place-stone S Pid p)]
         [(== state:counting) (check S Pid p)]
         [(== state:gameDone) (make-bundle S)]
         [else (error "Go <message>: server in unkown state" (server-state S))])]
      [else (bad-message M)])]
   [(msg: "pass") (make-pass S Pid)]
   [(msg: "continue") (play-again S)]
   [(msg: "next") (start-again S Pid)]
   [else (bad-message M)]))

(define (bad-message M)
  (error "Go: unknown message:" M))

(define (place-stone [S : Server][Pid : ID][pos : Exact-Number]) : (Bundle Server)
  (cond
    [(and (equal? (server-state S) state:playing)
          (equal? Pid (first-Pid S))
          (free-pos S pos))
     
     (define B0 (server-bord S))
     (define B1 (bord-add (server-bord S) pos Pid))
     (define-values (group around) (explore B1 pos))

     (define neighbours : (Listof (List (Listof Exact-Number)(Listof Exact-Number)))
       (let loop ([toTest (hash-ref around (next-Pid S) (λ () '()))]
                  [result : (Listof (List (Listof Exact-Number)(Listof Exact-Number))) '()])
         (cond
           [(null? toTest) result]
           [else
            (define p1 (car toTest))
            (define toTest+ (cdr toTest))
            (define-values (group free)(group-explore B1 p1))
            (loop (remove* (append group free) toTest+)
                  (cons (list group free) result))])))

     (define-values (B2 removed)
       (for*/fold ([B : Bord B1]
                   [removed : (Listof Exact-Number) '()])
                  ([L (in-list neighbours)]
                   [group (in-value (car L))]
                   [free (in-value (cadr L))])
         (if (null? free)
             (values (bord-remove B group) (append group removed))
             (values B removed))))

     (cond
       [(if (null? removed)
            (null? (hash-ref around #f));->nothing removed, new stone part of group without anything free
            (bord-equal? B2 (server-bord-1 S)));->removed stone, same situation as 2 ago
        (make-bundle S (mail:wrongPos S pos))]
       [else
        (switch-player
         (make-bundle
          (~> (struct-copy server S
                           [bord B2]
                           [bord-1 B0])
              (player-add-points _ Pid (length removed)))
          (mail:bordUpdate S Pid pos removed)))])]
    [else (make-bundle S (mail:wrongPos S pos))]))

(define (check [S : Server][Pid : ID][pos : Exact-Number]) : (Bundle Server)
  (match (server-state S)
    [(== state:counting)
     (define current-check (bord-get (server-bord-c S) pos))
     (cond
       ;if we have a stone at this position and the other player is NOT claiming it, we can't do anything
       [(and (equal? (bord-get (server-bord S) pos) Pid)
             (equal? current-check #f))
        (make-bundle S (mail:wrongPos S pos))]
       ;claim/declaim the position
       [else
        (define S1
          (~> (struct-copy server S
                           [bord-c (if current-check
                                       (bord-remove (server-bord-c S) (list pos))
                                       (bord-add (server-bord-c S) pos Pid))])
              (player-set-state _ (first-Pid S) player:check)
              (player-set-state _ (next-Pid S) player:check)))
        (make-bundle S1 (append (mail:playerState S1)(mail:bordCheck S1 pos)))])]
    [else (make-bundle S)]))

(define (make-pass [S : Server][Pid : ID]) : (Bundle Server)
  (match (server-state S)
    [(== state:playing)
     (cond
       [(equal? Pid (first-Pid S))
        (define S1 (struct-update server S [players (λ (L) (list (cadr L)(car L)))]))
        (cond
          [(equal? (player-state (first-player S1)) player:pass)
           (define count-bord (make-bord (bord-size (server-bord S1))))
           ;TODO - prepare counter bord so less works need to be done
           (define S2
             (~> (player-set-state S1 (first-Pid S1) player:check)
                 (player-set-state _  Pid player:check)
                 (struct-copy server _
                              [state state:counting]
                              [bord-c count-bord])))
           (make-bundle S2 (mail:playerState S2))]
          [else
           (define S2 (~> (player-set-state S1 (first-Pid S1) player:play)
                          (player-set-state _  (next-Pid S1) player:pass)))
           (make-bundle S2 (mail:playerState S2))])]
       [else (make-bundle S)])]
    [(== state:counting)
     (define S1 (player-set-state S Pid player:pass))
     (cond
       [(and (equal? (player-state (first-player S1)) player:pass)
             (equal? (player-state (next-player S1)) player:pass))
        (define S2
          (~> (struct-copy server S1 [state state:gameDone])
              calculate-check-points))
        (make-bundle S2
                     (append (mail:playerState S2)
                             (mail:gameDone S2)))]
       [else (make-bundle S1 (mail:playerState S1))])]
    [(== state:gameDone) (make-bundle S)]
    [else (error "Go <make-pass>: server in unkown state" (server-state S))]))

(define (play-again [S : Server]) : (Bundle Server)
  (define S1
    (~> (struct-copy server S [state state:playing])
        (player-set-state _ (first-Pid S) player:play)
        (player-set-state _ (next-Pid S) player:wait)))
  (make-bundle S1 (append (mail:playAgain S1)
                          (mail:playerState S1))))

(define (start-again [S : Server][Pid : ID]) : (Bundle Server)
  (cond
    [(equal? (server-state S) state:gameDone)
     (define S1 (player-set-state S Pid player:wait))
     (cond
       [(and (equal? (player-state (first-player S1)) player:wait)
             (equal? (player-state (next-player S1)) player:wait))
        (make-server+ (list (first-Pid S1)(next-Pid S1)))]
       [else (make-bundle S1 (mail:playerState S1))])]
    [else (make-bundle S)]))

(define (switch-player [BS : (Bundle Server)]) : (Bundle Server)
  ((inst bundle-update Server)
   BS
   #:u-state
   (λ ([S : Server])
     (~> (player-set-state S (first-Pid S) player:wait)
         (struct-update server _ [players (λ (L) (list (cadr L)(car L)))])
         (player-set-state _ (next-Pid S) player:play)))
   #:m-mails
   (λ ([S : Server]) (mail:playerState S))))

(define (calculate-check-points [S1 : Server]) : Server
  (define B (server-bord S1))
  (define P1 (first-Pid S1))
  (define P2 (next-Pid S1))
  
  (define-values (P1-score P2-score)
    (for/fold ([s1 : Nonnegative-Integer 0]
               [s2 : Nonnegative-Integer 0])
      ([(p v)(in-hash (bord-b (server-bord-c S1)))]
       #:when v)
      
      (define n (if (equal? (bord-get B p) #f) 1 2))
      (if (equal? v P1)
          (values (+ s1 n) s2)
          (values s1 (+ s2 n)))))
  (~> (player-add-points S1 P1 P1-score)
      (player-add-points _  P2 P2-score)))

;*******************************************************************************
;mail procedures
;*******************************************************************************
(define (pos->msg [pos : Exact-Number])(MSG [x (real-part pos)][y (imag-part pos)]))

(define (mail-to-all [S : Server][msg : Msg]) : (Listof Mail)
  (for/list ([P (in-list (server-players S))]
             #:unless (equal? (player-state P) player:dead))
    (mail (player-name P) msg)))

(define (mail:report [S : Server]) : (Listof Mail)
  (mail-to-all S (MSG "report"
                      [players (map player-name (server-players S))]
                      [bordSize (bord-size (server-bord S))])))

(define (mail:bordUpdate [S : Server][Pid : ID][pos : Exact-Number][removed : (Listof Exact-Number)]) : (Listof Mail)
  (mail-to-all S (MSG "bordUpdate"
                      [user Pid]
                      [new (pos->msg pos)]
                      [removed (map pos->msg removed)])))

(define (mail:bordCheck [S : Server][pos : Exact-Number]) : (Listof Mail)
  (mail-to-all S (MSG "bordCheck"
                      [pos (pos->msg pos)]
                      [state (bord-get (server-bord-c S) pos)])))

(define (mail:countpos [S : Server][Pid : ID][pos : (U Exact-Number (Listof Exact-Number))])
  (if (list? pos)
      (mail-to-all S (MSG "count" [user Pid][pos (map pos->msg pos)]))
      (mail-to-all S (MSG "count" [user Pid][pos (list (pos->msg pos))]))))

(define (mail:gameDone [S : Server]) : (Listof Mail)
  (define P1 (player-points (first-player S)))
  (define P2 (player-points (next-player S)))
  (mail-to-all S (if (= P1 P2)
                     (MSG "gameDone")
                     (MSG "gameDone" [winner (if (> P1 P2) (first-Pid S) (next-Pid S))]))))

(define (mail:playAgain [S : Server]) : (Listof Mail)
  (mail-to-all S (MSG "playAgain")))

(define (mail:playerState [S : Server]) : (Listof Mail)
  (apply
   append
   (for/list : (Listof (Listof Mail))
     ([P (in-list (server-players S))])
     (mail-to-all S (MSG "playerState"
                         [user (player-name P)]
                         [state (player-state P)]
                         [points (player-points P)])))))

(define (mail:wrongPos [S : Server][pos : Exact-Number]) : (Listof Mail)
  (mail-to-all S (MSG "wrongPos" [pos (pos->msg pos)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define SERF
  ((inst new-gameserver Server)
   "Go"
   empty-server
   make-server
   "../games/go/go.js"
   #:min-players 2
   #:max-players 2
   #:on-disconnect disconnect
   #:on-message message))

(module+ fcts
  (require (submod ".."))
  (provide (rename-out [make-server+ make-server]
                       [message+ message]))
  (define (make-server+ [Pids : (Listof ID)]) : (Bundle Server)
    (make-server empty-server Pids (λ (w #:name [name (id "error")]) (error "can't use new worker fct"))))
  (define (message+ [S : Server][Pid : ID][M : Msg]) : (Bundle Server)
    (message S Pid M (λ (w #:name [name (id "error")]) (error "can't use new worker fct")))))
