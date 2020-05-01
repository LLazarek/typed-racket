#lang typed/racket #:transient

;; Send a non-struct value across,
;; expect transient-assert error

(module u racket/base
  (struct posn [x y])
  (define origin 'not-posn)
  (provide origin (struct-out posn)))

(require/typed 'u
  (#:struct posn ((x : Real) (y : Real)))
  (origin posn))

