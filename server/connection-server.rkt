#lang typed/racket/base

(require racket/match)
(require "net.rkt"
         "util.rkt"
         (submod "util.rkt" connection))
(provide start-connection-server)

(define-logger ConServ)

(struct con ([name : ID]
             [nect : Connection]
             [evnt : (Evtof (U EOF String))]
             [worker/kill : (U Boolean (-> Void))])
  #:type-name Con)

(define #:forall (A)
  (start-connection-server [starter : (-> Worker-Fct (Bundle A))]
                           #:port [port : Positive-Integer PORT]
                           #:on-connection [connection : (-> A ID (Bundle A)) (λ ([S : A] N) (make-bundle S))]
                           #:on-disconnect [disconnect : (-> A ID (Bundle A)) (λ ([S : A] N) (make-bundle S))]
                           #:on-message [message : (-> A ID Msg (Bundle A))   (λ ([S : A] N M) (make-bundle S))]
                           #:on-tick [tick : (-> A (Bundle A))                (λ ([S : A]) (make-bundle S))]
                           #:tick-rate [tick-rate : Positive-Real +inf.0]) : (-> Void)
  ;*******************************************************************************
  ; Connection handling
  ;*******************************************************************************
  ;register unique name
  (define (get-unique-name [c : Connection]) : (Values (Option ID) (U Boolean (-> Void)))
    (define client-msg (ws-recv c))
    (cond
      [(eof-object? client-msg)
       (values #f #f)]
      [else
       (define JE (str->msg client-msg))
       (log-ConServ-debug "debug  # login attempt ~a" JE)
       (define name (and (tag? JE "login")(msg@ JE 'name)))
       (define wk (and (msg? JE)(msg@ JE 'worker)))
       (cond
         [(not (boolean? wk))
          (ws-send! c (msg->str (MSG "wrong format for worker: #t/#f")))
          (get-unique-name c)]
         [(and name (string? name)(not (get-connection (id name)))(boolean? wk))
          (ws-send! c (msg->str (MSG "nameOK")))
          (log-ConServ-debug "debug  # server->~a: ~a" name (MSG "nameOK"))
          (values (id name) wk)]
         [name
          (ws-send! c (msg->str (MSG "nameAlreadyUsed")))
          (get-unique-name c)]
         [(tag? JE "keepalive") (get-unique-name c)]
         [else
          (ws-send! c (msg->str (MSG "error" [msg "invalid login message"])))
          (get-unique-name c)])]))
  ;connection manager
  (: get-connection (-> ID (Option Con)))
  (: add-connection (-> Connection Void))
  (: remove-connection (-> ID Void))
  (: in-connections (-> (Sequenceof Con)))
  (: in-new-connections (-> (Sequenceof ID)))
  (: set-kill (-> ID (-> Void) Void))
  (define-values (get-connection add-connection remove-connection in-connections in-new-connections set-kill)
    (let ([C : (Immutable-HashTable String Con) #hash()]
          [N : (Listof ID) '()])
      (define (get-connection [name : ID]) : (Option Con)
        ((inst hash-ref String Con) C (string-upcase name) #f))
      (define (add-connection [c : Connection]) : Void
        (thread
         (λ ()
           (define-values (name worker/kill) (get-unique-name c))
           (when name
             (set! C (hash-set C
                               (string-upcase name)
                               (con name c (ws-recv-evt c) worker/kill)))
             (cond
               [worker/kill (log-ConServ-info "info   # created worker <~a>" name)]
               [else
                (set! N `(,@N ,name))
                (log-ConServ-info "info   # ~a logged in" name)
                (send-all (MSG "chat" [user name][message "<logged in>"]))]))))
        (void))
      (define (remove-connection [name : ID]) : Void
        (set! C (hash-remove C (string-upcase name))))
      (define (in-connections) : (Sequenceof Con)
        (in-list (hash-values C)))
      (define (in-new-connections) : (Sequenceof ID)
        (begin0 (in-list N)(set! N '())))
      (define (set-kill [n : ID][fct : (-> Void)]) : Void
        (define N (string-upcase n))
        (let loop ()
          (cond
            [(hash-ref C N #f)
             (set! C (hash-update C N (λ (nce)(struct-copy con nce [worker/kill fct]))))]
            [else (sleep .01)(loop)])))
      (values get-connection
              add-connection
              remove-connection
              in-connections
              in-new-connections
              set-kill)))
  ;*******************************************************************************
  ; The server
  ;*******************************************************************************
  ;start webserver to listen for new connections
  (define stop-server
    (serve #:port port
           #:selector "/game"
           #:fct (λ (c) (add-connection c))))
  ;*******************************************************************************
  ; Create a worker connection
  ;*******************************************************************************
  (define (handle-messages [c : Connection][M : (Option Msg)]) : Void
    (when M (ws-send! c (msg->str M))))
  (define  (start-worker [grunt : Worker]
                   #:name [nm : String "worker"])
    (define c (ws-connect (string->url (format "ws://127.0.0.1:~a/game" port))))
    (define name
      (let loop : ID ([i : Integer 0])
        (define name (format "~a ~a" nm i))
        (ws-send! c (msg->str (MSG "login" [name name][worker #t])))
        (define server-answer (ws-recv c))
        (cond
          [(eof-object? server-answer)
           (error "start-worker: Server died!")]
            [else
             (define M (str->msg server-answer))
             (if M (match-msg M
                              [(msg: "nameOK") (id name)]
                              [else (loop (+ i 1))])
                 (error "start-worker: Server went crazy!" server-answer))])))
    (define e (ws-recv-evt c))
    (define Δk 60000)
    ((worker-start grunt) name)
    (define thethread
      (thread
       (λ ()
         (let loop ([r (+ (current-inexact-milliseconds) (worker-tick-rate grunt))]
                    [k (+ (current-inexact-milliseconds) Δk)])
           (define time (current-inexact-milliseconds))
         (cond
           [(<= k time)
            (handle-messages c (MSG "keepalive"))
            (loop r (+ time Δk))]
           [(<= r time)
            (handle-messages c ((worker-tick grunt)))
            (loop (+ time TR) k)]
           [(sync/timeout MR e)
            =>
            (λ (str)
              (define server-msg str)
              (cond
                [(eof-object? server-msg)
                 (error "worker-loop: Server died!")]
                [else
                 (define msg (str->msg str))
                 (cond
                   [(tag? msg "keepalive")]
                   [(not msg) (log-ConServ-warning "warning# worker <~a> received unknown string: ~a" name str)]
                   [else (handle-messages c ((worker-message grunt) msg))])
                 (loop r k)]))]
           [else (loop r k)])))))
    (set-kill name (λ ()(kill-thread thethread)))
    name)
  ;*******************************************************************************
  ; helper functions for the message loop
  ;*******************************************************************************
  ;handele return bundle from the program running this server
  (define (handle-bundle [BS : (Bundle A)]) : A
    (defbundle (S M D) BS)
    (for ([m (in-list M)])
      (define nce (get-connection (mail-name m)))
      (if nce
          (cond
            [(with-handlers ([exn:fail? (λ (e)
                                          (log-ConServ-error
                                           "error  # Could not send - not a valid jsexpr\n\t~a"
                                           (mail-msg m))
                                          #f)])
               (msg->str (mail-msg m)))
             =>(λ (msg)
                 (match-define (con n c e s?) nce)
                 (log-ConServ-info "info   # server->~a: ~a" (if s? (format "<~a>" n) n) (mail-msg m))
                 (ws-send! (con-nect nce) msg))])
          (log-ConServ-error "error  # Could not send, not a registered player: ~a" (mail-name m))))
    (for* ([n (in-list D)]
           [nce (in-value (get-connection n))]
           #:when nce)
      (kick nce))
    S)
  ;kick a connection
  (define (kick [nce : Con]) : Void
    (match-define (con n c e s?) nce)
    (thread (λ ()(ws-close! c)))
    (remove-connection n)
    (cond
      [(equal? s? #f)
       (log-ConServ-info "info   # ~a logged out" n)
       (send-all (MSG "chat" [user n][message "<logged out>"]))]
      [(equal? s? #t)
       (error "kick: removing worker before kill switch has been set")]
      [else (s?)(log-ConServ-info "info   # stoped worker <~a>" n)]))
  ;helper for sending messages
  (define (send-all [msg : Msg]) : Void
    (for ([nce (in-connections)]
          #:unless (con-worker/kill nce))
      (ws-send! (con-nect nce) (msg->str msg))))
  (define TR (max tick-rate .1))
  (define MR (min .01 (/ TR 10)))
  ;*******************************************************************************
  ; The message loop
  ;*******************************************************************************
  ;listen for message on the open connections and dispatch the event to
  ;the program running this server
  (define threadloop
    (thread
     (λ()
       (let loop ([S : A (handle-bundle (starter start-worker))]
                  [t (+ TR (current-inexact-milliseconds))])
         (define t+ (current-inexact-milliseconds))
         (cond
           [(<= t t+)
            (loop (handle-bundle (tick S))
                  (+ t+ TR))]
           [else
            (define S1
              (for/fold : A
                ([S0 : A S])
                ([nce (in-connections)])
                (define S
                  (for/fold : A
                    ([S : A S0])
                    ([n (in-new-connections)])
                    (handle-bundle (connection S n))))
                (match-define (con n c e s?) nce)
                (cond
                  [(sync/timeout MR e)
                   =>
                   (λ (str)
                     (cond
                       [(eof-object? str)
                        (kick nce)
                        (handle-bundle (disconnect S n))]
                       [else
                        (define msg (str->msg str))
                        (cond
                          [(tag? msg "keepalive")
                           (log-ConServ-debug "debug  # keeping ~a alive" (if s? (format "<~a>" n) n))
                           (when s? (ws-send! c (msg->str (MSG "keepalive"))))
                           S]
                          [(tag? msg "chat")
                           (case (msg@ msg 'message)
                             [("<leave>")(handle-bundle (message S n (MSG "leaveGame")))]
                             [else
                              (send-all (MSG "chat"
                                             [user n]
                                             [message (msg@ msg 'message (λ () "no-message"))]))
                              S])]
                          [(not msg) (log-ConServ-warning "warning# connection-server received from ~a an unknown string:\n\t~a" (if s? (format "<~a>" n) n) str) S]
                          [else
                           (log-ConServ-info "info   # ~a->server: ~a" (if s? (format "<~a>" n) n) msg)
                           (handle-bundle (message S n msg))])]))]
                  [else S])))
            (loop S1 t)])))))
  ;*******************************************************************************
  ; return a function to end the connection-server
  ;*******************************************************************************
  (define (kill-server)
    (kill-thread threadloop)
    (stop-server))
  kill-server)

#;(module+ test
  (start-connection-server
   (λ ([w : Worker-Fct])
     (thread
      (λ ()
        (let loop ()
          (w (λ (Nid) (make-post #f)))
          (sleep (random))
          (loop))))
     (make-bundle #f))))