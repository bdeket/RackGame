#lang racket/base

(require rackunit
         threading
         (for-syntax racket/base
                     racket/syntax))
(require "../../server/util-untyped.rkt"
         (submod "datastructs.rkt" untyped)
         (submod "server.rkt" fcts))

(define a (id "a"))(define b (id "b"))
(define B0
  (~> (make-bord 9)
      (bord-add _ 1+0i a)
      (bord-add _ 0+1i a)
      (bord-add _ 1+1i a)
      (bord-add _ 2+2i a)))
(check-equal? (let-values ([(grp free)(group-explore B0 1+i)])
                (list grp free))
              (list '(1 0+1i 1+1i)
                    '(2 0 0+2i 1+2i 2+1i)))

(define B1
  (~> (make-bord 9)
      (bord-add _ +1 a)
      (bord-add _ +i a)
      (bord-add _ +0 b)))
(check-equal? (let-values ([(grp free)(group-explore B1 1+0i)])
                (list grp free))
              (list '(1)
                    '(1+1i 2)))
(check-equal? (let-values ([(grp free)(group-explore B1 0)])
                (list grp free))
              (list '(0)
                    '()))
(check-equal? (let-values ([(grp free)(explore B1 0)])
                (list grp free))
              (list '(0)
                    #hash((#f . ())
                          ("a" . (0+1i 1)))))

(define-syntax (do stx)
  (syntax-case stx ()
    [(do i j Pid M)
     (with-syntax ([Si (format-id #'i "S~a" (syntax->datum #'i))]
                   [Mi (format-id #'i "M~a" (syntax->datum #'i))]
                   [Sj (format-id #'i "S~a" (syntax->datum #'j))])
       (syntax/loc stx (begin (defbundle (Si Mi) (message Sj Pid M))(set! S Si)(newline)(printf "~a: ~a ~a\n" i Pid M) Mi)))]
    [(do i Pid M)
     (with-syntax ([j (- (syntax->datum #'i) 1)])
      (syntax/loc stx (do i j Pid M)))]))

(defbundle (S0 M0) (make-server (list a b)))(newline)S0 M0
(define S S0)
;*******************************************************************************
(do 1 a (MSG "place" [x 1][y 1]))
(check-equal? (first-Pid S) b)
(check-equal? (next-Pid S) a)
(check-equal? (bord-get (server-bord S) 1+1i) a)
(check-equal? (player-state (first-player S)) player:play)
(check-equal? (player-state (next-player S)) player:wait)

;*******************************************************************************
(do 2 b (MSG "place" [x 1][y 1]))
(check-equal? (first-Pid S) b)
(check-equal? (next-Pid S) a)
(check-equal? (bord-get (server-bord S) 1+1i) a)
(check-equal? (player-state (first-player S)) player:play)
(check-equal? (player-state (next-player S)) player:wait)

;*******************************************************************************
(do 3 b (MSG "place" [x 1][y 2]))
(check-equal? (first-Pid S) a)
(check-equal? (next-Pid S) b)
(check-equal? (bord-get (server-bord S) 1+1i) a)
(check-equal? (bord-get (server-bord S) 1+2i) b)
(check-equal? (player-state (first-player S)) player:play)
(check-equal? (player-state (next-player S)) player:wait)

;*******************************************************************************
(do 4 a (MSG "pass"))
(check-equal? (first-Pid S) b)
(check-equal? (next-Pid S) a)
(check-equal? (bord-get (server-bord S) 1+1i) a)
(check-equal? (bord-get (server-bord S) 1+2i) b)
(check-equal? (player-state (first-player S)) player:play)
(check-equal? (player-state (next-player S)) player:pass)

;*******************************************************************************
(do 5 b (MSG "place" [x 2][y 1]))
(check-equal? (first-Pid S) a)
(check-equal? (next-Pid S) b)
(check-equal? (bord-get (server-bord S) 1+1i) a)
(check-equal? (bord-get (server-bord S) 1+2i) b)
(check-equal? (bord-get (server-bord S) 2+1i) b)
(check-equal? (player-state (first-player S)) player:play)
(check-equal? (player-state (next-player S)) player:wait)

;*******************************************************************************
(do 6 a (MSG "place" [x 0][y 1]))
(check-equal? (bord-get (server-bord S) 0+1i) a)

;*******************************************************************************
(do 7 b (MSG "place" [x 0][y 2]))
(check-equal? (bord-get (server-bord S) 0+2i) b)

;*******************************************************************************
(do 8 a (MSG "place" [x 1][y 0]))
(check-equal? (bord-get (server-bord S) 1+0i) a)

;*******************************************************************************
(do 9 b (MSG "place" [x 2][y 0]))
(check-equal? (bord-get (server-bord S) 2+0i) b)

;*******************************************************************************
(do 10 a (MSG "place" [x 0][y 0]))
(check-equal? (bord-get (server-bord S) 0+0i) #f)

;*******************************************************************************
(do 11 a (MSG "place" [x 1][y 3]))
(check-equal? (bord-get (server-bord S) 1+3i) a)

;*******************************************************************************
(do 12 b (MSG "place" [x 0][y 0]))
(check-equal? (bord-get (server-bord S) 0+0i) b)
(check-equal? (bord-get (server-bord S) 0+1i) #f)
(check-equal? (bord-get (server-bord S) 1+0i) #f)
(check-equal? (bord-get (server-bord S) 1+1i) #f)
(check-equal? (player-points (next-player S)) 3)

;*******************************************************************************
(do 13 a (MSG "place" [x 3][y 3]))
(check-equal? (bord-get (server-bord S) 3+3i) a)

;*******************************************************************************
(do 14 b (MSG "place" [x 3][y 2]))
(check-equal? (bord-get (server-bord S) 3+2i) b)

;*******************************************************************************
(do 15 a (MSG "place" [x 2][y 4]))
(check-equal? (bord-get (server-bord S) 3+2i) b)

;*******************************************************************************
(do 16 b (MSG "place" [x 2][y 3]))
(check-equal? (bord-get (server-bord S) 3+2i) b)

;*******************************************************************************
(do 17 a (MSG "place" [x 2][y 2]))
(check-equal? (bord-get (server-bord S) 2+2i) a)
(check-equal? (bord-get (server-bord S) 2+3i) #f)
(check-equal? (player-points (next-player S)) 1)

;*******************************************************************************
(do 18 b (MSG "place" [x 2][y 3]))
(check-equal? (bord-get (server-bord S) 2+3i) #f)

;*******************************************************************************
(do 19 b (MSG "place" [x 0][y 3]))
(check-equal? (bord-get (server-bord S) 0+3i) b)

;*******************************************************************************
(do 20 a (MSG "pass"))

;*******************************************************************************
(do 21 b (MSG "place" [x 2][y 3]))
(check-equal? (bord-get (server-bord S) 2+3i) b)
(check-equal? (player-points (next-player S)) 4)

;*******************************************************************************
(do 22 a (MSG "pass"))

;*******************************************************************************
(do 23 b (MSG "pass"))
(check-equal? (first-Pid S) a)
(check-equal? (next-Pid S) b)
(check-equal? (player-state (first-player S)) player:check)
(check-equal? (player-state (next-player S)) player:check)

;*******************************************************************************
(do 24 b (MSG "place" [x 4][y 3]))

;*******************************************************************************
(do 25 b (MSG "place" [x 4][y 3]))

;*******************************************************************************
(do 26 b (MSG "continue"))
(check-equal? (first-Pid S) a)
(check-equal? (next-Pid S) b)
(check-equal? (player-state (first-player S)) player:play)
(check-equal? (player-state (next-player S)) player:wait)

