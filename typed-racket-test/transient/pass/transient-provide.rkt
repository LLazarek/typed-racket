#lang racket/base

;; Test providing value to untyped

(module a typed/racket/base #:transient
        (provide f)
        (define (f (x : (Boxof Integer)))
          (unbox x)))

(require 'a rackunit)
(check-exn #rx"transient-assert"
  (lambda () (f 0)))
