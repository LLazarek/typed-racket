#lang typed/racket/base #:transient

(struct (A) s ([f : A]) #:type-name S)

(define si : (S String) (s "foo"))
(ann (s-f si) String)

(define-struct/exec exec ()
  [(λ (e x) (add1 x)) : (Exec Real -> Real)]
  #:type-name Exec)

((ann (exec) Exec) 3)
