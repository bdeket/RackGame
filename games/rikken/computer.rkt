#lang racket/base

(require racket/match
         racket/list)
(require "util.rkt")

(provide make-computer)

(struct world (name cards round))
(define empty-world (world "" '() '()))
;*******************************************************************************
; new computer
;*******************************************************************************
(define (make-computer new-worker nm)
  (new-worker #:name nm
              (create-worker (world nm '() '())
                             #:start (λ(W nm) (make-post (struct-copy world W [name nm])))
                             #:message message)))
;*******************************************************************************
; message handling
;*******************************************************************************
(define (message W M)
  (match-msg M
    [(msg: s-key:cards [cards Cs])
     (make-post (struct-copy world W [cards (map msg->card Cs)]))]
    [(msg: s-key:selectBid [options Os][lastCard C])
     (make-post W (MSG w-key:selectBid [bid (car Os)]))]
    [(msg: s-key:selectCard)
     (define C
       (cond
         [(empty? (world-round W)) (car (shuffle (world-cards W)))]
         [else
          (define F (filter (λ (x)(equal? (card-suit x)(card-suit (car (world-round W))))) (world-cards W)))
          (cond
            [(empty? F) (car (shuffle (world-cards W)))]
            [else (car (shuffle F))])]))
     (make-post W (MSG w-key:selectCard [card (card->msg C)]))]
    [(msg: s-key:cardPlayed [card c][user _])
     (define C (msg->card c))
     (make-post
      (struct-update world W
                     [round (λ (Cs) `(,@Cs ,C))]
                     [cards (λ (Cs) (remove C Cs))]))]
    [(msg: s-key:lastRound +) (make-post (struct-copy world W [round '()]))]
    [(msg: s-key:gameDone) (make-post (struct-copy world W [cards '()][round '()]))]
    ;ignore
    [(msg: s-key:players +) (make-post W)]
    [(msg: s-key:playerState +) (make-post W)]
    [(msg: s-key:gameType +) (make-post W)]
    [else
     (printf "rikken-computer: unknown message ~a\n" M)
     (make-post W)]))
