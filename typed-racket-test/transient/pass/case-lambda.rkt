#lang racket/base

(module t typed/racket/base #:transient
  (: f0 (case-> (-> Symbol Symbol)))
  (define f0
    (case-lambda
      [(x) x]))

  (: f1 (case-> (-> Symbol Symbol)
                (-> String String String)))
  (define f1
    (case-lambda
      [(x) x]
      [(y z) (string-append y z)]))
  (provide f0 f1))

(require 't rackunit)

(check-not-exn
  (lambda () (f0 'ok)))

(check-exn exn:fail:contract?
  (lambda () (f0 42)))

(check-not-exn
  (lambda () (f1 'ok)))

(check-not-exn
  (lambda () (f1 "a" "b")))

(check-exn exn:fail:contract?
  (lambda () (f1 42)))

(check-exn exn:fail:contract?
  (lambda () (f1 "a" 'b)))

;; ---

(module u racket/base
  (define f1
    (case-lambda
      [(x) x]
      [(y z) 'oops]))
  (provide f1))

(module t1 typed/racket/base #:transient
  (require/typed (submod ".." u)
    (f1 (case-> (-> Symbol Symbol)
                (-> String String String))))
  (require typed/rackunit)
  (check-exn exn:fail:contract?
    (lambda () (f1 "a" "b"))))
(require 't1)