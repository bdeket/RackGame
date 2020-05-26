#lang racket/base

(require racket/list
         racket/match
         threading)
(require "util.rkt"
         "computer.rkt")

(provide SERF)

(define (cycle lst)`(,@(cdr lst) ,(car lst)))
(define (cycle-to lst Pid)
  (define-values (A B)(splitf-at lst (λ (e) (not (equal? e Pid)))))
  (append B A))

(struct server (players ;hash of players
                order ;(listof player-id)
                gameStarter ;first player of game
                round ;cards of the current round
                lastRound ;cards of the last round
                trump ;trump suit of this game
                ace ;following ace of this game
                type ;type of game of this game
                lastcard ;last card of this deck
                pointmodifier ;points of this game counting couble?
                ) #:transparent)
(define (server-player S Pid)(hash-ref (server-players S) Pid))
(define-syntax-rule (player-update S Pid [key fct] ...)
  (struct-update server S
                 [players (λ (H) (hash-update H Pid
                                              (λ (P) (struct-update player P [key fct]...))))]))
(define (server-currentPlayer S)(car (server-order S)))

(struct player (name state cards call team rounds points comp?) #:transparent)
(define (new-player Pid)(player Pid "waiting" '() #f #f 0 0 #f))
(define (new-comp Pid)(player Pid "waiting" '() #f #f 0 0 #t))

(define (make-deck)
  (shuffle
   (for*/list ([s (in-range 4)]
               [n (in-range 13)])
     (card s n))))
(define (card-sort Cs)
  (sort Cs (λ (c1 c2)
             (if (< (card-suit c1)(card-suit c2)) #t
                 (if (= (card-suit c1)(card-suit c2))
                     (< (card-number c1)(card-number c2))
                     #f)))))

;*******************************************************************************
;new Server
;*******************************************************************************
(define empty-server (server #hash() '() #f '() '() #f #f #f #f 1))
(define (make-server S Pids create-worker)
  (define PH0 (for/hash ([Pid (in-list Pids)])
                (values Pid (new-player Pid))))
  (define PH1
    (cond
      [(= 4 (hash-count PH0)) PH0]
      [(< 4 (hash-count PH0)) (error "To many players for Rikken" Pids)]
      [else
       (for/fold ([PH PH0])
                 ([i (in-naturals 1)]
                  #:break (<= 4 (hash-count PH)))
         (define name (make-computer create-worker "computer"))
         (if (hash-ref PH name #f) Pids (hash-set PH name (new-comp name))))]))
  (define O (hash-keys PH1))
  (define S1 (server PH1 O (car O) '() '() #f #f #f #f 1))
  (new-game (make-bundle S1 (msg:players S1)) #f))

;*******************************************************************************
; Disconnect
;*******************************************************************************
(define (disconnect S Pid create-worker)
  (define name (make-computer create-worker "computer"))
  (define P (server-player S Pid))
  (define S+
    (struct-update server S
                   [players
                    (λ (H)
                      (hash-set (hash-remove H Pid)
                                name (struct-copy player P
                                                  [name name]
                                                  [comp? #t])))]))
  (make-bundle S+ (msg:playerState S+ name)))

;*******************************************************************************
; Message handling
;*******************************************************************************
(define (message S Pid M create-worker)
  (define P (server-player S Pid))
  (match M
    [(msg: w-key:selectCard [card (h: [suit s][number n])])
     (define C (card s n))
     (cond
       [(and (equal? (player-state P) player:selectCard)
             (can-play? C (player-cards P) (server-round S)))
        (define S+ (~> (struct-update server S [round (λ (Cs) `(,@Cs (,C . ,Pid)))])
                       (player-update _ Pid [cards (λ (Cs) (remove C Cs))])))
        (next-in-round (make-bundle S+ (msg:cardPlayed S+ Pid C)))]
       [else (make-bundle S)])]
    [(msg: w-key:selectBid [bid bid])
     (cond
       [(and (equal? (player-state P) player:selectBid)
             (member bid (player-call P)))
        (handle-bid S Pid P bid)]
       [else (make-bundle S)])]
    [(msg: w-key:selectTrump [suit s])
     (cond
       [(and (equal? (player-state P) player:selectTrump)
             (member s (server-trump S)))
        (handle-trump S Pid P s)]
       [else (make-bundle S)])]
    [(msg: w-key:selectAce [suit s])
     (cond
       [(and (equal? (player-state P) player:selectAce)
             (member s (server-ace S)))
        (handle-ace S Pid P s)]
       [else (make-bundle S)])]
    [(msg: w-key:next)
     (define S+ (player-update S Pid [state (λ (_) player:next)]))
     (cond
       [(for/and ([(Pid P)(in-hash (server-players S+))]
                  #:unless (player-comp? P))
          (equal? "next" (player-state P)))
        (new-game (make-bundle S+) #f)]
       [else
        (make-bundle S+ (msg:playerState S+ Pid))])]
    [else
     (printf "rikserver-unhandled message ~a\n" M)
     (make-bundle S)]))

;*******************************************************************************
; Game steps
;*******************************************************************************
;prepare new game, by preparing deck
(define (new-game BS double?)
  (~> (if double?
          (bundle-update BS #:u-state (λ (S) (struct-update server S [type (λ (_)#f)][pointmodifier (λ (x)(* x 2))])))
          (bundle-update BS #:u-state (λ (S) (struct-copy server S [type #f][pointmodifier 1]))))
      deal-cards
      start-bid))
(define (deal-cards BS)
  (defbundle (S M) BS)
  (define deck (make-deck))
  (define deal
    (let loop ([ans '()]
               [Cs deck])
      (cond
        [(empty? Cs) ans]
        [else
         (define-values (D D+)(split-at Cs 13))
         (loop (cons (card-sort D) ans) D+)])))
  (define O+ (cycle (cycle-to (server-order S) (server-gameStarter S))))
  (bundle-update
   BS
   #:u-state (λ (S)
               (struct-copy server S
                            [players (for/hash ([(Pid P)(in-hash (server-players S))]
                                                [cs (in-list deal)])
                                       (values Pid (struct-copy player P
                                                                [cards cs]
                                                                [team #f]
                                                                [call #f]
                                                                [rounds 0])))]
                            [order O+][gameStarter (car O+)]
                            [lastcard  (last deck)]
                            [round '()]
                            [lastRound '()]
                            [trump #f][ace #f][type #f]))
   #:m-mails (λ (S)(msg:ownCards S))))
;*******************************************************************************
; Pre-game play (bidding)
;*******************************************************************************
(define (start-bid BS)
  (bundle-update
   BS
   #:u-state (λ (S)
               (~> (struct-update server S
                                  [players (λ (H)
                                             (for/hash ([(Pid P)(in-hash H)])
                                               (values Pid (struct-copy player P [call bid-list]))))])
                   check-troel
                   (player-update _ (server-currentPlayer S)
                                  [state (λ (_) player:selectBid)])))
   #:m-mails (λ (S) `(,@(msg:playerState S (server-currentPlayer S))
                      ,(msg1:bid S)))))
(define (check-troel S)
  (define Pid+
    (for/first ([(Pid P) (in-hash (server-players S))]
                #:when (= 3 (for/sum ([c (in-list (player-cards P))]#:when (= (card-number c) 12))1)))
      Pid))
  (cond
    [Pid+
     (define ace (car (remove*
                       (for/list ([c (in-list (player-cards (server-player S Pid+)))]
                                  #:when (= (card-number c) 12))
                         (card-suit c))
                       '(0 1 2 3))))
     (struct-update server S
                    [players (λ (H)
                               (for/hash ([(Pid P)(in-hash H)])
                                 (values
                                  Pid
                                  (struct-update player P
                                                 [call (λ (L) (if (equal? Pid Pid+)
                                                                  (cons bid:troel (remove* (list bid:pas bid:rik bid:rikaf bid:miserie bid:abondance) L))
                                                                  (remove* (list bid:rik bid:rikaf bid:miserie bid:abondance) L)))]
                                                 [state (λ (o)(if (equal? Pid Pid+)bid:troel o))]))))]
                    [ace (λ (_) ace)])]
    [else S]))
(define (handle-bid S Pid P bid)
  (define S0 (player-update S Pid [call (λ (_) bid)]))
  (define S1
    (cond
      [(equal? bid bid:pas) S0]
      [(equal? bid bid:troel)
       (struct-copy server S0 [type bid])]
      [(equal? bid bid:soloslim)
       (struct-update server S0
                      [type (λ (_) bid)]
                      [players
                       (λ (H)
                         (for/hash ([(Pid P) (in-hash H)])
                           (cond
                             [(list? (player-call P))
                              (struct-update player P
                                             [call (λ (Cs) bid:pas)])]
                             [else (values Pid P)])))])]
      [else
       (struct-update server S0
                      [type (λ (_) bid)]
                      [players
                       (λ (H)
                         (for/hash ([(Pid P) (in-hash H)])
                           (cond
                             [(list? (player-call P))
                              (cond
                                [(equal? bid bid:rik)
                                 (values Pid (struct-update player P
                                                            [call (λ (Cs)
                                                                    (cons bid:pas (cons bid:rikaf (cdr (member bid Cs)))))]))]
                                [(member bid (list bid:rikaf bid:abondance))
                                 (values Pid (struct-update player P
                                                            [call (λ (Cs)
                                                                    (cons bid:pas (cdr (member bid Cs))))]))]
                                [(member bid (list bid:miserie bid:openmiserie))
                                 (values Pid (struct-update player P
                                                            [call (λ (Cs)
                                                                    (cons bid:pas (member bid Cs)))]))]
                                [else (error "handle-bid" bid)])]
                             [else (values Pid P)])))])]))
  (bidwinner? S1))
(define (bidwinner? S)
  (define S0 (~> (player-update S (server-currentPlayer S) [state (λ (_) player:waiting)])
                 (struct-update server _ [order (λ (O) (cycle O))])))
  (cond
    [(for/and ([(Pid P)(in-hash (server-players S0))])(not (list? (player-call P))))
     (cond
       [(member (server-type S0) (list bid:miserie bid:openmiserie))
        (start-first-round (make-bundle S0))]
       [(member (server-type S0) (list bid:rik bid:abondance bid:soloslim))
        (define Pid+ (for/first ([(Pid P)(in-hash (server-players S0))]#:when (equal? (server-type S0)(player-call P)))Pid))
        (define S1 (~> (player-update S0 Pid+ [state (λ (_) player:selectTrump)])
                       (struct-update server _
                                      [order (λ (O)(cycle-to O Pid+))]
                                      [trump (λ (_) '(0 1 2 3))])))
        (make-bundle S1 `(,@(msg:playerState+call S1 (server-currentPlayer S))
                          ,@(msg:playerState+call S1 Pid+)
                          ,(msg1:selectTrump S1 Pid+)))]
       [(equal? (server-type S0) bid:rikaf)
        (define Pid+ (for/first ([(Pid P)(in-hash (server-players S0))]#:when (equal? (server-type S0)(player-call P)))Pid))
        (handle-trump S0 Pid+ (server-player S0 Pid+) (card-suit (server-lastcard S0)))]
       [(equal? (server-type S0) bid:troel)
        (define Pid (for/first ([(Pid P)(in-hash (server-players S0))]
                                #:when (equal? (player-call P) (server-type S0)))
                      Pid))
        (start-first-round
         (make-bundle
          (~> (player-update S0 (for/first ([(Pid P)(in-hash (server-players S0))]
                                            #:when (member (card (server-ace S0) 12) (player-cards P)))
                                  Pid) [team (λ (_) #t)])
              (player-update _ Pid [team (λ (_) #t)]))))]
       [else (new-game (make-bundle S0 (msg:playerState+call S0 (server-currentPlayer S))) #t)])]
    [else
     (define S++ (player-update S0 (server-currentPlayer S0) [state (λ (_) player:selectBid)]))
     (make-bundle S++
                  `(,@(msg:playerState+call S++ (server-currentPlayer S))
                    ,@(msg:playerState+call S++ (server-currentPlayer S++))
                    ,(msg1:bid S++)))]))
(define (handle-trump S Pid P s)
  (define S0 (struct-copy server S [trump s]))
  (cond
    [(equal? (server-type S0) bid:rik)
     (define S1 (player-update S0 Pid [state (λ (_) player:selectAce)]))
     (define lst (remove* (cons s
                                (for/list ([c (in-list (player-cards P))]
                                           #:when (= (card-number c) 12))
                                  (card-suit c)))
                          '(0 1 2 3)))
     (define S2 (struct-copy server S1 [ace lst]))
     (make-bundle S2 `(,@(msg:playerState S2 Pid)
                       ,(msg1:selectAce S2 Pid)))]
    [else (start-first-round (make-bundle S0))]))
(define (handle-ace S Pid P s)
  (start-first-round
   (make-bundle
    (~> (player-update S (for/first ([(Pid P)(in-hash (server-players S))]
                                     #:when (member (card s 12) (player-cards P)))
                           Pid) [team (λ (_) #t)])
        (player-update _ Pid [team (λ (_) #t)])
        (struct-copy server _ [ace s])))))

;*******************************************************************************
; Normal gameplay
;*******************************************************************************
(define (start-first-round BS)
  (start-round
   (bundle-update BS
                  #:u-state (λ (S)(struct-update server S
                                               [order (λ (O)(cycle-to O (server-gameStarter S)))]))
                  #:m-mails (λ (S)(msg:gameType S)))))
(define (start-round BS)
  (defbundle (S) BS)
  (define FP (car (server-order S)))
  (cond
    [(empty? (player-cards (server-player S FP)))
     (defbundle (S) BS)
     (assign-game-points BS)]
    [else
     (define S+
       (struct-copy server (player-update S FP [state (λ (_) player:selectCard)])
                    [lastRound (map car (server-round S))]
                    [round '()]))
     (bundle-update BS
                    #:u-state (λ (S)
                              (struct-copy server (player-update S FP [state (λ (_) player:selectCard)])
                                           [lastRound (map car (server-round S))]
                                           [round '()]))
                    #:m-mails (λ (S) `(,@(msg:lastRound S) ,@(msg:players S) ,(msg1:selectCard S))))]))

(define (next-in-round BS)
  (defbundle (S) BS)
  (define O (server-order S))
  (define O+ (cycle O))
  (define S+ (~> S
                 (player-update _ (car O) [state (λ (S) player:waiting)])
                 (struct-copy server _ [order O+])))
  (cond
    [(equal? (length O) (length (server-round S)))
     (end-round (bundle-update BS
                               #:s-state S+
                               #:m-mails (λ (S) (msg:playerState S (car O)))))]
    [else
     (bundle-update BS
                    #:s-state (player-update S+ (car O+) [state (λ (_) player:selectCard)])
                    #:m-mails (λ (S) `(,@(msg:playerState S (car O))
                                       ,@(msg:playerState S (car O+))
                                       ,(msg1:selectCard S))))]))

(define (end-round BS)
  (defbundle (S) BS)
  (define win (round-winner (server-round S) (server-trump S)))
  (define O+ (cycle-to (server-order S) win))
  (start-round
   (bundle-update
    (assign-round-points
     (bundle-update BS #:u-state (λ (S) (struct-copy server S [order O+])))
     win)
    #:m-mails (λ (S)(msg:playerState S (car (server-order S)))))))

(define (can-play? C PCs RCs)
  (and (member C PCs)
       (or (empty? RCs)
           (let ([S0 (card-suit (caar RCs))])
             (or (equal? (card-suit C) S0)
                 (empty? (filter (λ (C)(equal? (card-suit C) S0)) PCs)))))))
(define (round-winner RCs T)
  (define FCS (card-suit (caar RCs)))
  (define trumps (filter (λ (CP) (equal? (card-suit (car CP)) T)) RCs))
  (cdr (argmax (λ (CP) (card-number (car CP)))
               (if (empty? trumps)
                   (filter (λ (CP) (equal? (card-suit (car CP)) FCS)) RCs)
                   trumps))))

;*******************************************************************************
; rikken-specific
;*******************************************************************************
(define (assign-round-points BS win)
  (defbundle (S0) BS)
  (cond 
    [(and (equal? (server-type S0) bid:troel)
          (assoc (card (server-ace S0) 12) (server-round S0)))
     =>
     (λ (c&p)
       (define win (cdr c&p))
       (bundle-update BS
                      #:u-state (λ (S) (~> (player-update S win [rounds add1])
                                         (struct-update server _
                                                        [trump (λ (_) (server-ace S))]
                                                        [order (λ (O) (cycle-to O win))])))
                      #:m-mails (λ (S)(msg:gameType S #t))))]
    [(and (member (server-type S0) (list bid:rik bid:rikaf))
          (assoc (card (server-ace S0) 12) (server-round S0)))
     (bundle-update BS #:m-mails (λ (S)(msg:gameType S #t)))]
    [else (bundle-update BS #:u-state (λ (S)(player-update S win [rounds add1])))]))
(define (assign-game-points BS)
  (defbundle (S0) BS)
  (define S1
    (match (server-type S0)
      [(or ($ bid:rik) ($ bid:rikaf))
       (define points
         (for/sum ([(Pid P)(in-hash (server-players S0))]
                   #:when (player-team P))
           (player-rounds P)))
       (cond
         [(< points 8)
          (define d (- 8 points))
          (struct-update server S0
                         [players
                          (λ (H)
                            (for/hash ([(Pid P)(in-hash H)])
                              (values
                               Pid
                               (cond
                                 [(equal? (player-team P) #f)
                                  (struct-update player P [points (λ (p) (+ p d))])]
                                 [(equal? (server-type S0)(player-call P))
                                  (struct-update player P [points (λ (p) (- p (* 2 d)))])]
                                 [else P]))))])]
         [else
          (struct-update server S0
                         [players
                          (λ (H)
                            (for/hash ([(Pid P)(in-hash H)])
                              (values
                               Pid
                               (struct-update player P [points
                                                        (λ (p) ((if (player-team P) + -)
                                                                p (- points 7)))]))))])])]
      [($ bid:troel)
       (define points
         (for/sum ([(Pid P)(in-hash (server-players S0))]
                   #:when (player-team P))
           (player-rounds P)))
       (define d (if (<= points 9) (- points 9) (- points 7)))
       (struct-update server S0
                      [players
                       (λ (H)
                         (for/hash ([(Pid P)(in-hash H)])
                           (values
                            Pid
                            (struct-update player P [points
                                                     (λ (p) ((if (player-team P) + -)
                                                             p d))]))))])]
      [(or ($ bid:miserie) ($ bid:openmiserie))
       (define pts (if (equal? (server-type S0) bid:miserie) 3 7))
       (define-values (fail ok)
         (for/fold ([fail '()]
                    [ok '()])
                   ([(Pid P)(in-hash (server-players S0))]
                    #:when (equal? (player-call P)(server-type S0)))
           (if (= (player-rounds P) 0)
               (values fail (cons Pid ok))
               (values (cons Pid fail) ok))))
       (define oks (length ok))
       (define fs (length fail))
       (define-values (o d- d+)
         (match (list fs oks)
           [(or '(0 1) '(1 1) '(2 1) '(3 1)) (values (* -1 pts) (* -1 pts) (* 3 pts))]
           [(or        '(0 2) '(1 2) '(2 2)) (values (* -1 pts) (* -1 pts) (* 1 pts))]
           [(or               '(0 3) '(1 3)) (values (* -3 pts) (* -3 pts) (* 1 pts))]
           [(or '(1 0))                      (values (*  1 pts) (* -3 pts) (* 0 pts))]
           [(or        '(2 0))               (values (*  1 pts) (* -1 pts) (* 0 pts))]
           [(or               '(3 0))        (values (*  3 pts) (* -1 pts) (* 0 pts))]
           [(or                      '(4 0)) (values 0 0 0)]
           [else (values 0 0 0)]))
       (struct-update server S0
                      [players (λ (H)
                                 (for/hash ([(Pid P) (in-hash H)])
                                   (values
                                    Pid
                                    (struct-update player P
                                                   [points (λ (p)
                                                             (cond
                                                               [(member Pid fail)(+ p d-)]
                                                               [(member Pid ok)(+ p d+)]
                                                               [else (+ p o)]))]))))])]
      [(or ($ bid:abondance) ($ bid:soloslim))
       (define minR (if (equal? (server-type S0) bid:abondance) 9 13))
       (define pts (if (equal? (server-type S0) bid:abondance) 5 10))
       (define user
         (for/first ([(Pid P)(in-hash (server-players S0))]
                     #:when (equal? (player-call P)(server-type S0)))
           P))
       (define d (if (<= (player-rounds user) minR) (- pts) pts))
       (struct-update server S0
                      [players
                       (λ (H)
                         (for/hash ([(Pid P)(in-hash H)])
                           (values
                            Pid
                            (struct-update player P [points
                                                     (λ (p)
                                                       (if (equal? P user)
                                                           (+ p (* 3 d))
                                                           (- p d)))]))))])]
      [else S0]))
  (bundle-update
   BS
   #:s-state S1
   #:m-mails (λ (S)(append (msg:lastRound S)(msg:players S)(msg:gameDone S)))))

;*******************************************************************************
; mail procedures
;*******************************************************************************
(define (mail-to-all S msg)
  (for/list ([Pid (in-list (server-order S))])
    (mail Pid msg)))

(define (player->msg P) (MSG [user (player-name P)][state (player-state P)][type (player-comp? P)][round (player-rounds P)][point (player-points P)]))

(define (msg:gameDone S)(mail-to-all S (MSG s-key:gameDone)))
(define (msg:players S)
  (for/list ([(Pid P) (in-hash (server-players S))])
    (define O (cycle-to (server-order S) Pid))
    (mail Pid (MSG s-key:players [players (map (λ (Pid) (player->msg (server-player S Pid))) O)]))))
(define (msg:playerState S Pid)
  (define P (server-player S Pid))
  (mail-to-all S (MSG s-key:playerState [user Pid][state (player-state P)][type (player-comp? P)])))
(define (msg:playerState+call S Pid)
  (define P (server-player S Pid))
  (mail-to-all S (MSG s-key:playerState [user Pid]
                      [state (player-state P)]
                      [type (player-comp? P)]
                      [bid (if (list? (player-call P)) "" (player-call P))])))
(define (msg:lastRound S)(mail-to-all S (MSG s-key:lastRound [cards (map card->msg (server-lastRound S))])))
(define (msg:cardPlayed S Pid C)(mail-to-all S (MSG s-key:cardPlayed [user Pid][card (card->msg C)])))
(define (msg:gameType S [team? #f])
  (define users (for/list ([(Pid P)(in-hash (server-players S))] #:when (equal? (player-call P)(server-type S))) Pid))
  (define team (if team?
                   (for/list ([(Pid P)(in-hash (server-players S))]
                              #:when (player-team P)
                              #:unless (member Pid users))
                     Pid)
                   '()))
  (match (server-type S)
    [(or ($ bid:miserie) ($ bid:openmiserie))
     (mail-to-all S (MSG s-key:gameType [type (server-type S)][users users]))]
    [($ bid:troel)
     (if (server-trump S)
         (mail-to-all S (MSG s-key:gameType [type (server-type S)][trump (server-trump S)][users users][team team]))
         (mail-to-all S (MSG s-key:gameType [type (server-type S)][users users])))]
    [(or ($ bid:abondance) ($ bid:soloslim))
     (mail-to-all S (MSG s-key:gameType [type (server-type S)][trump (server-trump S)][users users]))]
    [(or ($ bid:rik) ($ bid:rikaf))
     (if team?
         (mail-to-all S (MSG s-key:gameType [type (server-type S)][trump (server-trump S)][ace (server-ace S)][users users][team team]))
         (mail-to-all S (MSG s-key:gameType [type (server-type S)][trump (server-trump S)][ace (server-ace S)][users users])))]))

(define (msg:ownCards S) (map (λ (Pid)(msg1:ownCards S Pid)) (server-order S)))

(define (msg1:ownCards S Pid)
  (mail Pid (MSG s-key:cards [cards (map card->msg (player-cards (server-player S Pid)))])))
(define (msg1:bid S)
  (define Pid (server-currentPlayer S))
  (define P (server-player S Pid))
  (mail Pid (MSG s-key:selectBid [options (player-call P)][lastCard (card->msg (server-lastcard S))])))
(define (msg1:selectCard S) (mail (server-currentPlayer S) (MSG s-key:selectCard)))
(define (msg1:selectTrump S Pid)(mail Pid (MSG s-key:selectTrump [trump (server-trump S)])))
(define (msg1:selectAce S Pid)(mail Pid (MSG s-key:selectAce [ace (server-ace S)])))

;*******************************************************************************
; bundle the server
;*******************************************************************************
(define SERF
  (new-gameserver "Rikken"
                  empty-server
                  make-server
                  "../games/rikken/rikken.js"
                  #:min-players 1
                  #:max-players 4
                  #:on-disconnect disconnect
                  #:on-message message
                  ))

;*******************************************************************************
; testing
;*******************************************************************************
;(require racket/trace) (trace message)
;(require racket/trace)(trace end-round)
;(require racket/trace)(trace play-first-possible)
;(require racket/trace)(trace round-winner)
;(require racket/trace)(trace play-first-possible)
;(require racket/trace)(trace bidwinner?)
(module+ test
    (define (play-first-possible S [P (server-player S (server-currentPlayer S))])
      (message S
               (player-name P)
               (MSG "selectCard"
                    [card (card->msg
                           (for/first ([C (in-list (player-cards P))]
                                       #:when (can-play? C (player-cards P) (server-round S)))
                             C))])
               void))
    (require (for-syntax racket/base racket/syntax))
    (define-syntax (do stx)
      (syntax-case stx (+)
        [(_ id expr)
         (with-syntax ([S (format-id stx "S~a" (syntax-e #'id))]
                       [M (format-id stx "M~a" (syntax-e #'id))])
           (syntax/loc stx
             (begin (println 'S)
                    (define-values (S M) (let ([BS expr]) (values (bundle-state BS)(bundle-mails BS))))
                    S '-> M (newline))))]
        [(_ + id expr)
         (with-syntax ([S (format-id stx "S~a" (syntax-e #'id))]
                       [M (format-id stx "M~a" (syntax-e #'id))])
           (syntax/loc
               stx
             (begin (println 'S)
                    (define-values (S M)
                      (let ([BS expr])
                        (values (bundle-state BS)(bundle-mails BS)))))))]))
    (random-seed 0)
    (do + 0 (make-server empty-server '("B" "W" "D" "M") void))
    ;(do + 0.1 (disconnect S0 "B" void))
    ;(do + 0.2 (make-server '("B")))
    ;a game of players playing the first available card
    (for/fold ([S S0])
              ([i (in-range 1 (+ 1 (* 4 13)))])
      (with-handlers ([exn:fail? (λ (e) (println i)(println S)(raise e))])
        (bundle-state (play-first-possible S))))
    ;a game with 1players & 3computers playing the first available card
    #;(for/fold ([S S0.2])
              ([i (in-range 1 (+ 1 (* 1 13)))])
      (with-handlers ([exn:fail? (λ (e) (println i)(println S)(raise e))])
        (bundle-state (play-first-possible S))))

  
    )
