#lang racket/load

;; Test typed-untyped interaction with channels

(module typed typed/racket/shallow
  (: ch (Channelof (Boxof Integer)))
  (define ch (make-channel))
  (: putter (-> Thread))
  (define (putter)
    (thread (λ () (channel-put ch (box 3)))))
  (provide putter ch))

(require 'typed)
(putter)
(set-box! (channel-get ch) "not an integer")

