#lang racket/base

(require racket/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))
(require "util.rkt")

(provide (all-from-out "util.rkt")
         struct-update structhashstruct-copier structhashstruct-updater
         MSG match-msg $ $$ h: msg: defbundle)

(define-syntax (struct-update stx)
  (syntax-case stx ()
    [(struct-update type s [elem fct]...)
     (with-syntax ([(elems ...)
                    (map (λ (e) (format-id #'type "~a-~a" #'type e))
                         (syntax-e #'(elem ...)))])
       (syntax/loc stx (struct-copy type s [elem (fct (elems s))]...)))]))
(define-syntax (structhashstruct-updater stx1)
  (syntax-case stx1 ()
    [(_ name type1 field type2)
     (with-syntax ([getter (format-id stx1 "~a-~a" #'type1 #'field)])
       (syntax/loc
           stx1
         (define-syntax (name stx2)
           (syntax-case stx2 ()
             [(_ S ID [key fct] (... ...))
              (quasisyntax/loc
                  stx2
                (struct-copy
                 type1 S
                 [field (hash-set (getter S) ID
                                  (struct-update #,(replace-context stx2 #'type2)
                                                 (hash-ref (getter S) ID)
                                                 [key fct](... ...)))]))]))))]))
(define-syntax (structhashstruct-copier stx1)
  (syntax-case stx1 ()
    [(_ name type1 field type2)
     (with-syntax ([getter (format-id stx1 "~a-~a" #'type1 #'field)])
       (syntax/loc
           stx1
         (define-syntax (name stx2)
           (syntax-case stx2 ()
             [(_ S ID [key val] (... ...))
              (quasisyntax/loc
                  stx2
                (struct-copy
                 type1 S
                 [field (hash-set (getter S) ID
                                  (struct-copy #,(replace-context stx2 #'type2)
                                               (hash-ref (getter S) ID)
                                               [key val](... ...)))]))]))))]))
(define-syntax (MSG stx)
  (syntax-parse stx
    [(_ [T expr]...) (syntax/loc stx (make-immutable-hash (list (cons `T expr)...)))]
    [(_ thetag [T expr]...) (syntax/loc stx (msg thetag (MSG [T expr]...)))]))
(define-syntax (match-msg stx)
  (syntax-case stx ()
    [(_ M [a body ...]...)
     (syntax/loc stx
         (match M
           [a body ...]...))]))
(define-match-expander $
  (λ (stx)
    (syntax-case stx ()
      [($ x) #'(? (λ (y)(equal? x y)) _)])))
(define-match-expander $$
  (λ (stx)
    (syntax-case stx ()
      [($ x fct) #'(? (λ (x)fct) x)])))
(define-match-expander h:
  (λ (stx)
    (syntax-case stx (+)
      [(_ [El Pat]... +)#'(hash-table (`El Pat)... (ingoretag ignorevalue)(... ...))]
      [(_ [El Pat]...)  #'(hash-table (`El Pat)...)])))
(define-match-expander msg:
  (λ (stx)
    (syntax-case stx (+)
      [(_ tag [El Pat]... +)#'(msg ($ tag) (hash-table (`El Pat)... (ingoretag ignorevalue)(... ...)))]
      [(_ tag [El Pat]...)  #'(msg ($ tag) (hash-table (`El Pat)...))])))
(define-syntax (defbundle stx)
  (syntax-case stx ()
    [(_ (B M D) BS)(syntax/loc stx (define-values (B M D)(let ([it BS])(match it [(bundle A B C) (values A B C)][else (values it '() '())]))))]
    [(_ (B M) BS)(syntax/loc stx (define-values (B M)(let ([it BS])(match it [(bundle A B C) (values A B)][else (values it '())]))))]
    [(_ (B) BS)(syntax/loc stx (define-values (B) (let ([it BS])(match it [(bundle A B C) A][else it]))))]))

