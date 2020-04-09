#lang racket/base

(module u racket/base
  (struct posn (x y))
  (provide (struct-out posn)))

(module t typed/racket/base
  (require/typed/provide (submod ".." u)
    (#:struct posn ((x : Real) (y : Real)))))

(module s typed/racket/base #:transient
  (require racket/match
           (submod ".." t))
  (: f (-> posn Void))
  (define (f p)
    (match p
     ((posn x y) (void (+ x y)))
     (_ (void)))))

(require 's)
