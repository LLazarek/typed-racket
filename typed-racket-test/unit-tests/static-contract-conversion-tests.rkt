#lang racket/base

(require "test-utils.rkt" "evaluator.rkt"
         rackunit
         (only-in racket/contract contract?)
         syntax/srcloc syntax/location
         (for-syntax
           syntax/parse
           racket/base
           typed-racket/private/type-contract
           typed-racket/static-contracts/instantiate
           typed-racket/types/abbrev
           typed-racket/types/numeric-tower))

(provide tests)
(gen-test-main)

(begin-for-syntax
  (define-splicing-syntax-class type-enforcement-flag
    #:attributes (value)
    (pattern (~or #:deep
                  (~seq))
      #:with value 'deep)
    (pattern (~seq #:optional)
      #:with value 'optional)
    (pattern (~seq #:shallow)
      #:with value 'shallow)))

(define-syntax t/sc
  (syntax-parser
    [(_ e:expr te-flag:type-enforcement-flag)
     (syntax/loc #'e
       (test-case
         (format "Conversion:~a" (quote-line-number e))
         (with-check-info (['type 'e]
                           ['location (build-source-location-list (quote-srcloc e))]
                           ['enforcement-mode 'te-flag.value])
           (phase1-phase0-eval
             (define sc
                (type->static-contract e (lambda (#:reason _) #f) #:enforcement-mode 'te-flag.value))
             (if sc
                 #`(with-check-info (['static '#,sc])
                     (phase1-phase0-eval
                       (define ctc (cadr
                                     (instantiate '#,sc
                                       (lambda (#:reason _) (error "static-contract could not be converted to a contract")))))
                       #,#'#`(with-check-info (['contract '#,ctc])
                          (define runtime-contract #,ctc)
                          (check-pred contract? runtime-contract))))
                 #'(fail-check "Type could not be converted to a static contract"))))))]))

(define-syntax t/fail
  (syntax-parser
    [(_ e:expr (~optional (~seq #:typed-side typed-side) #:defaults ([typed-side #'#t])) te-flag:type-enforcement-flag)
     #`(test-case (format "~a" 'e)
         (define sc
           (phase1-phase0-eval
             (let/ec exit
               #`'#,(type->static-contract e (lambda (#:reason _) (exit #'#f)) #:typed-side typed-side #:enforcement-mode 'te-flag.value))))
         (when sc
           (with-check-info (['static sc])
             (fail-check "Type was incorrectly converted to contract"))))]))

(define tests
  (test-suite "Conversion Tests"
   (test-suite "Guarded Tests"
    (t/sc (-Number . -> . -Number))
    (t/sc (cl->* (-> -Symbol)
                 (-Symbol . -> . -Symbol)))
    (t/sc (cl->* (-> -Symbol)
                 (-Symbol -Symbol . -> . -Symbol)))
    (t/sc (cl->* (-Symbol . -> . -Symbol)
                 (-Symbol -Symbol . -> . -Symbol)))
    (t/sc (-Promise -Number))
    (t/sc (-struct-property (-> Univ -Number Univ) #f))
    (t/sc (-struct-property (-> -Self -Number) #f))
    (t/fail (-struct-property (-> -Self -Number) #f)  #:typed-side #f)
    (t/sc (-lst -Symbol))
    (t/sc -Boolean)
    (t/sc Univ)
    (t/sc (-set Univ))
    (t/sc (-poly (a) (-lst a)))
    (t/fail ((-poly (a) (-vec a)) . -> . -Symbol))
    (t/fail (-poly (a) (-lst a)) #:typed-side #f)
    (t/sc (-mu a (-lst a)))
    (t/sc (-mu a (-box a)))
    (t/sc (-mu sexp (Un -Null -Symbol (-pair sexp sexp) (-vec sexp) (-box sexp))))
    (t/sc (-mu a (-> a a)))
    (t/sc (-seq -Symbol))
    ;; HashTables with non-flat keys and values (Issue 625)
    ;;   https://github.com/racket/typed-racket/issues/625
    (t/sc (-Mutable-HT (-vec -Symbol) (-vec -Symbol)))
    (t/sc (-Immutable-HT (-vec -Symbol) (-vec -Symbol)))
    (t/sc (-Weak-HT (-vec -Symbol) (-vec -Symbol)))
    (t/sc (-HT (-vec -Symbol) (-vec -Symbol)))
    ;; These tests for unit static contracts are insufficient, but
    ;; in order to test Unit types the signature environment must be
    ;; set up correctly. More complex cases of compilation to unit/c
    ;; contracts are tested by integration tests.
    (t/sc (-unit null null null (-values (list -String))))
    (t/sc (-unit null null null (-values (list -Symbol -String))))
    (t/fail (-unit null null null ManyUniv)))

   (test-suite "Shallow Tests"
    (t/sc (-Number . -> . -Number) #:shallow)
    (t/sc (cl->* (-> -Symbol)
                 (-Symbol . -> . -Symbol)) #:shallow)
    (t/sc (-Promise -Number) #:shallow)
    (t/sc (-struct-property (-> -Self -Number) #f) #:shallow)
    (t/sc (-struct-property (-> -Self -Number) #f)  #:shallow)
    (t/sc (-set Univ) #:shallow)
    (t/sc ((-poly (a) (-vec a)) . -> . -Symbol) #:shallow)
    (t/sc (-poly (a) (-lst a)) #:shallow)
    (t/sc (-mu sexp (Un -Null -Symbol (-pair sexp sexp) (-vec sexp) (-box sexp))) #:shallow)
    (t/sc (-Mutable-HT (-vec -Symbol) (-vec -Symbol)) #:shallow)
    (t/sc (-HT (-vec -Symbol) (-vec -Symbol)) #:shallow)
    (t/sc (-unit null null null (-values (list -Symbol -String))) #:shallow)
    (t/sc (-unit null null null ManyUniv) #:shallow))

   (test-suite "Optional (typing) tests"
    ;; Optional always succeeds with any/sc
    (t/sc (-Number . -> . -Number) #:optional)
    (t/sc (-struct-property (-> -Self -Number) #f) #:optional)
    (t/sc (-struct-property (-> -Self -Number) #f)  #:optional)
    (t/sc (-set Univ) #:optional)
    (t/sc (-HT (-vec -Symbol) (-vec -Symbol)) #:optional)
    (t/sc (-unit null null null ManyUniv) #:optional))
   ))
