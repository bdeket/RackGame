#lang typed/racket/base

(require/typed net/url
               [#:opaque Url url?]
               [string->url (-> String Url)])
(require/typed net/rfc6455
               [#:opaque Connection ws-conn?]
               [ws-send! (-> Connection String Void)]
               [ws-close! (-> Connection Void)]
               [ws-recv (-> Connection (U EOF String))]
               [ws-recv-evt (-> Connection (Evtof (U EOF String)))]
               [ws-connect (-> Url Connection)])

(module inner racket/base
  (require net/rfc6455)
  (provide serve)
  (define (serve #:port [port 8081]
                 #:selector [slc "/"]
                 #:fct [fct void])
    (ws-serve* #:port port
               (ws-service-mapper
                [slc [(#f) fct]]))))

(require/typed (submod "." inner)
               [serve (->* ()
                           (#:port Positive-Integer
                            #:selector String
                            #:fct (-> Connection Void))
                           (-> Void))])
(provide Connection
         string->url
         ws-send!
         ws-close!
         ws-recv
         ws-recv-evt
         ws-connect
         serve)
