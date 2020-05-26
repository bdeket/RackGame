#lang typed/racket/base

(require racket/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/strip-context
                     racket/list))

(provide PORT dummyserver struct-update structhashstruct-copier structhashstruct-updater)
(provide (struct-out msg) Msg MSGval MSGitem MSG msg@ tag? tag match-msg $ $$ h: msg:)
(provide (struct-out bundle) Bundle defbundle make-bundle bundle-update bundle-combine
         (struct-out mail) Mail ID id
         (struct-out post) Post make-post)
(provide (struct-out worker) Worker create-worker Worker-Fct
         (struct-out gameserver) Gameserver new-gameserver
         (struct-out gameinstance) Gameinstance)


(define PORT 8081)
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
(module+ test-ext
  (require (submod ".."))
  (provide (all-defined-out))
  (struct a ([aa : Number]))
  (struct b ([bb : (HashTable Number a)]))
  (define B (b `#hash((1 . ,(a 1)))))
  (structhashstruct-updater aba b bb a)
  (aba B 1 [aa add1]))
(module+ test
  (require (submod ".." test-ext))
  (aba B 1 [aa add1]))
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
(module+ test-ext2
  (require (submod ".."))
  (provide (all-defined-out))
  (struct a ([aa : Number]))
  (struct b ([bb : (HashTable Number a)]))
  (define B (b `#hash((1 . ,(a 1)))))
  (structhashstruct-copier aba b bb a)
  (aba B 1 [aa 2]))
#;(module+ test
    (require (submod ".." test-ext2))
    (aba B 1 [aa 2]))

;*******************************************************************************
; json messages
;*******************************************************************************
(define-type JSval (U String Boolean Real))
(define-type MSGitem (U JSval (Listof JSval) MSGval (Listof MSGval)))
(define-type MSGval (Immutable-HashTable Symbol MSGitem))
(struct msg ([tag : String]
             [val : MSGval])
  #:transparent)(define-type Msg msg)
(define-syntax (MSG stx)
  (syntax-parse stx
    [(_ [T expr]...) (syntax/loc stx (ann ((inst make-immutable-hash Symbol MSGitem) (list (cons `T expr)...)) MSGval))]
    [(_ thetag [T expr]...) (syntax/loc stx (msg thetag (MSG [T expr]...)))]))
(define (msg@ [M : Msg] [at : Symbol] [e : (-> MSGitem)(λ () #f)]) : MSGitem
  (if msg (hash-ref (msg-val M) at e) (e)))
(define (tag? [msg : (Option Msg)][T :  String]) (and (msg? msg)(string=? (msg-tag msg) T)))
(define (tag [msg : (Option Msg)][e : (-> (Option String))(λ ()#f)])(if (msg? msg) (msg-tag msg) (e)))


(define-type LSTitem (U JSval (Listof JSval) LSTval (Listof LSTval)))
(define-type LSTval (Listof (List Symbol LSTitem)))
(define-type LSTmsg (List String LSTval))
(define (mi-> [M : MSGitem]) : LSTitem
  (cond
    [(list? M)
     (if (not (null? M))
         (if (hash? (car M))
             ((inst map LSTval MSGval) mv-> M)
             M)
         M)]
    [(hash? M) (mv-> M)]
    [else M]))
(define (mv-> [M : MSGval]) : LSTval
 ((inst sort (List Symbol LSTitem) Symbol)
  (hash-map M (λ ([a : Symbol][b : MSGitem]) : (List Symbol LSTitem)
               (list a (mi-> b))))
  symbol<? #:key car))
(define (msg->list [M : Msg]) : LSTmsg
  (list (msg-tag M)
        (mv-> (msg-val M))))

(define-syntax (match-msg stx)
  (syntax-case stx ()
    [(_ M [a body ...]...)
     (syntax/loc stx
       (match (msg->list M)
         [a body ...]...))]))
(define-match-expander $
  (λ (stx)
    (syntax-case stx ()
      [($ x) #'(? (λ (y)(equal? x y)) _)])))
(define-match-expander $$
  (λ (stx)
    (syntax-case stx ()
      [($ x fct) #'(? (λ (x)fct) x)])))
(begin-for-syntax
  (define (pat-sorter stx)
    (datum->syntax stx
                   (sort (syntax-e stx)
                         symbol<? #:key (λ (x) (syntax-e (car (syntax-e x)))))))
  (define (insert-ignores stx)
    (cons
     (datum->syntax stx `(list ,(gensym) ,(gensym)))
     (cons
      (datum->syntax stx '...)
      (apply
       append
       (for/list ([s (in-list (syntax-e (with-syntax ([((E P) ...) stx])#'((list `E P) ...))))]
                  [i (in-naturals 1)])
         (list s (datum->syntax stx `(list ,(gensym) ,(gensym))) (datum->syntax stx '...))))))))
(define-match-expander h:
  (λ (stx)
    (syntax-case stx (+)
      [(_ [El Pat]... +)
       (with-syntax ([(EP+ ...) (insert-ignores (pat-sorter #'((El Pat) ...)))])
         #'(list EP+ ...))]
      [(_ [El Pat]...)
       (with-syntax ([((E+ P+) ...) (pat-sorter #'((El Pat) ...))])
         #'(list (list 'E+ P+)...))])))
(define-match-expander msg:
  (λ (stx)
    (syntax-case stx (+)
      [(_ tag [El Pat]... +)(syntax/loc #'tag (list ($ tag) (h: [El Pat]... +)))]
      [(_ tag [El Pat]...)  (syntax/loc #'tag (list ($ tag) (h: [El Pat]... )))])))

(module+ test
  (require racket/match)
  (match-msg (MSG "halo" [a (+ 1 2)][c "vier"])
             [(msg: "halo" +) "match-one ok"])
  (match-msg (MSG "halo" [a (+ 1 2)][c "vier"])
             [(msg: "halo" [c ($ "vier")][a ($$ b (number? b))]) "match-two ok"])
  (match-msg (MSG "halo" [a (+ 1 2)][c "vier"][b "ietanders"])
             [(msg: "halo" [c ($ "vier")][a ($$ b (number? b))] +) "math-three ok"]))
  
;*******************************************************************************
; bundle
;*******************************************************************************
(define-new-subtype ID (id String))
(struct (A) bundle ([state : A]
                    [mails : (Listof Mail)]
                    [drops : (Listof ID)])
  #:transparent)(define-type Bundle bundle)
(struct mail ([name : ID]
              [msg : Msg])
  #:transparent)(define-type Mail mail)
(struct (A) post ([state : A]
                  [mail : (Option Msg)])
  #:transparent)(define-type Post post)

(: make-bundle (All (A) (->* (A) ((Listof Mail)(Listof ID)) (Bundle A))))
(define (make-bundle state [mails '()][drops '()])
  (bundle state mails drops))
(define #:forall (A) (make-post [S : A][M : (Option Msg) #f]) : (Post A) (post S M))

(define-syntax (defbundle stx)
  (syntax-case stx ()
    [(_ (B M D) BS)(syntax/loc stx (define-values (B M D)(let ([it BS])(match it [(bundle A B C) (values A B C)][else (values it '() '())]))))]
    [(_ (B M) BS)(syntax/loc stx (define-values (B M)(let ([it BS])(match it [(bundle A B C) (values A B)][else (values it '())]))))]
    [(_ (B) BS)(syntax/loc stx (define-values (B) (let ([it BS])(match it [(bundle A B C) A][else it]))))]))

(struct notset () #:type-name Notset)
(define notit (notset))
(: bundle-update (All (A) (->* ((Bundle A))
                               (#:s-state (U Notset A)
                                #:u-state (U Notset (-> A A))
                                #:a-mails (U Notset (Listof Mail))
                                #:m-mails (U Notset (-> A (Listof Mail)))
                                #:u-mails (U Notset (-> A (Listof Mail) (Listof Mail)))
                                #:a-drops (U Notset (Listof ID))
                                #:m-drops (U Notset (-> A (Listof ID)))
                                #:u-drops (U Notset (-> A (Listof ID) (Listof ID))))
                               (Bundle A))))
(define (bundle-update BS
                       #:s-state [ss notit]
                       #:u-state [us notit]
                       #:a-mails [am notit]
                       #:m-mails [mm notit]
                       #:u-mails [um notit]
                       #:a-drops [ad notit]
                       #:m-drops [md notit]
                       #:u-drops [ud notit])
  (defbundle (S M D) BS)
  (define S1
    (let ([S0 (if (notset? ss) S ss)])
      (if (notset? us) S0 (us S0))))
  (define M1
    (let* ([M0 (if (notset? am) M (append M am))]
           [M1 (if (notset? mm) M0 (append M0 (mm S1)))])
      (if (notset? um) M1 (um S1 M1))))
  (define D1
    (let* ([D0 (if (notset? ad) D (append D ad))]
           [D1 (if (notset? md) D0 (append D0 (md S1)))])
      (if (notset? ud) D1 (ud S1 D1))))
  (bundle S1 M1 D1))
(define #:forall (A)
  (bundle-combine [BS : (Bundle A)] . [BSs : (Bundle A) *]) : (Bundle A)
  (defbundle (S0 M0 D0) BS)
  (define-values (S1 M1 D1)
    (for/fold : (Values A (Listof Mail) (Listof ID))
      ([S S0][M M0][D D0])
      ([BS (in-list BSs)])
      (defbundle (S+ M+ D+) BS)
      (values S+ (append M M+)(append D D+))))
  (bundle S1 M1 D1))

;*******************************************************************************
; gameserver
;*******************************************************************************
(struct worker ([start : (-> ID (Option Msg))]
                [message : (-> Msg (Option Msg))]
                [tick : (-> (Option Msg))]
                [tick-rate : Positive-Real]))(define-type Worker worker)
(define #:forall (A)
  (create-worker [empty : A]
                 #:start [start : (-> A ID (Post A)) (λ (S Pid) (make-post S))]
                 #:message [message : (-> A Msg (Post A)) (λ (S M)(make-post S))]
                 #:tick [tick : (-> A (Post A)) (λ (S) (make-post S))]
                 #:tick-rate [tick-rate : Positive-Real +inf.0]) : Worker
  (define STATE empty)
  (worker (λ ([Pid : ID])
            (define P (start STATE Pid))
            (set! STATE (post-state P))
            (post-mail P))
          (λ ([M : Msg])
            (define P (message STATE M))
            (set! STATE (post-state P))
            (post-mail P))
          (λ ()
            (define P (tick STATE))
            (set! STATE (post-state P))
            (post-mail P))
          tick-rate))
(define-type Worker-Fct (->* (Worker)
                             (#:name String)
                             ID))
(define-type Start-Fct (All (A) (-> A (Listof ID) Worker-Fct (Bundle A))))
(define-type Disconnect-Fct (All (A) (-> A ID Worker-Fct (Bundle A))))
(define-type Message-Fct (All (A) (-> A ID Msg Worker-Fct (Bundle A))))
(define-type Tick-Fct (All (A) (-> A Worker-Fct (Bundle A))))

(struct gameserver ([name : String]
                    [min : Positive-Integer]
                    [max : Positive-Integer]
                    [tick-rate : Positive-Real]
                    [world : Path-String]
                    [toInstance : (-> Gameinstance)]
                    ))(define-type Gameserver gameserver)
(struct gameinstance ([start : (-> (Listof ID) Worker-Fct
                                   (Values (Listof Mail)(Listof ID)))]
                      [disconnect : (-> ID Worker-Fct
                                        (Values (Listof Mail)(Listof ID)))]
                      [message : (-> ID Msg Worker-Fct
                                     (Values (Listof Mail)(Listof ID)))]
                      [tick : (-> Worker-Fct
                                  (Values (Listof Mail)(Listof ID)))]
                      ))(define-type Gameinstance gameinstance)

(define #:forall (A)
  (new-gameserver [name : String]
                  [empty : A]
                  [start : (Start-Fct A)]
                  [world : Path-String]
                  #:min-players [min : Positive-Integer 1]
                  #:max-players [max : Positive-Integer 100]
                  #:on-disconnect [disconnect : (Disconnect-Fct A) (λ ([G : A]Pid CW) (make-bundle G))]
                  #:on-message [message : (Message-Fct A) (λ ([G : A]Pid M CW) (make-bundle G))]
                  #:on-tick [tick : (Tick-Fct A) (λ ([G : A]CW) (make-bundle G))]
                  #:tick-rate [tick-rate : Positive-Real +inf.0]) : Gameserver
  (gameserver name
              min
              max
              tick-rate
              world
              (λ ()
                (define STATE empty)
                (gameinstance (λ ([Pids : (Listof ID)][CW : Worker-Fct])
                                (defbundle (S M D) (start STATE Pids CW))
                                (set! STATE S)(values M D))
                              
                              (λ ([Pid : ID][CW : Worker-Fct])
                                (defbundle (S M D) (disconnect STATE Pid CW))
                                (set! STATE S)(values M D))
                              
                              (λ ([Pid : ID][M : Msg][CW : Worker-Fct])
                                (define BS (message STATE Pid M CW))
                                (set! STATE (bundle-state BS))(values (bundle-mails BS)(bundle-drops BS)))
                              
                              (λ ([CW : Worker-Fct])
                                (defbundle (S M D) (tick STATE CW))
                                (set! STATE S)(values M D))))))

(define dummyserver (new-gameserver "Dummy" #f (λ (S Pid CW) (make-bundle S)) "./games/dummy.js"))
;*******************************************************************************
; convert msg to json and back
;*******************************************************************************
(module+ connection
  (require/typed json
                 [jsexpr->string (-> MSGitem String)]
                 [string->jsexpr (-> String MSGitem)])

  (provide msg->str str->msg)
  
  (define (msg->str [m : Msg]) : String
    (jsexpr->string (hash-set (msg-val m) 'tag (msg-tag m))))

  (define (str->msg [s : String]) : (Option Msg)
    (define H (and (string? s)
                   (with-handlers ([exn:fail? (λ (e) #f)])
                     (string->jsexpr s))))
    (if (hash? H)
        (let ([T (hash-ref H 'tag #f)])
          (if (string? T)
              (msg T (hash-remove H 'tag))
              #f))
        #f)))