#lang typed/racket #:transient
(struct (A) S ([f : A]))

(define-type T (∩ (S Nonnegative-Integer) (S Nonpositive-Integer)))
