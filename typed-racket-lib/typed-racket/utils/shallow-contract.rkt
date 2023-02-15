#lang racket/base

;; Extra contracts and tools for the Shallow runtime,
;; which is based on Michael Vitousek's Transient semantics

(provide
  procedure-arity-includes-keywords?
  shallow-and/c
  shallow-or/c
  shallow-shape-check
  raise-shallow-check-error

  arg-cast
  ;; (-> value from value)
  ;; Apply to function args, records a "cast"

  make-shallow-provide-contract)

(require
  (only-in racket/contract blame-positive make-flat-contract)
  racket/lazy-require
  typed-racket/utils/shallow-contract-struct)

(lazy-require ;; avoid circular reference to type-contract.rkt
  (typed-racket/utils/shallow-filter (value-type-match? sexp->type)))

;; ---------------------------------------------------------------------------------------------------

;; procedure-arity-includes-keywords? : (-> procedure? (listof keyword?) (listof keyword?) boolean?)
;; Returns true if the procedure accepts calls that supply all mandatory keywords
;; and some optional keywords --- in the sense of racket/contract arity checking.
;; + function must declare all optional keywords as optional
;; + function may declare all mandatory keywords as either mandatory or optional
(define (procedure-arity-includes-keywords? f mand-kw* opt-kw*)
  (define-values [f-mand-kw* f-opt-kw*] (procedure-keywords f))
  ;; note: f-opt-kw* = (sort f-opt-kw* keyword<?)
  (define mand-ok/extra*
    ;; subtract f's mandatory keywords from the expected list (must be a prefix)
    (let loop ((expected-kw* mand-kw*)
               (actual-kw* f-mand-kw*))
      (cond
        [(null? actual-kw*)
         expected-kw*]
        [(null? expected-kw*)
         #f]
        [else
         (and (eq? (car expected-kw*) (car actual-kw*))
              (loop (cdr expected-kw*) (cdr actual-kw*)))])))
  (and mand-ok/extra*
       (let loop ((expected-kw* (sort (append mand-ok/extra* opt-kw*) keyword<?))
                  (actual-kw* f-opt-kw*))
         ;; match the remaining keywords against f's optionals
         (cond
          ((null? expected-kw*)
           #true)
          ((or (null? actual-kw*) (keyword<? (car expected-kw*) (car actual-kw*)))
           #false)
          ((eq? (car actual-kw*) (car expected-kw*))
           (loop (cdr expected-kw*) (cdr actual-kw*)))
          (else ;#(keyword<? actual expected)
           (loop expected-kw* (cdr actual-kw*)))))))

(define (shallow-and/c . pred*)
  (lambda (x)
    (let loop ((p?* pred*))
      (if (null? p?*)
        #true
        (if ((car p?*) x)
          (loop (cdr p?*))
          #false)))))

(define (shallow-or/c . pred*)
  (lambda (x)
    (let loop ((p?* pred*))
      (if (null? p?*)
        #false
        (if ((car p?*) x)
          #true
          (loop (cdr p?*)))))))

(define (shallow-shape-check val pred ty-str ctx from)
  (blame-map-set! val ty-str from)
  (if (pred val)
    val
    (begin
      (print-blame-map)
    (raise-shallow-check-error val ty-str ctx from))))

(define (raise-shallow-check-error val ty ctx from)
  (log-transient-info "blame tracing ~a ~a~n" from (blame-compress-key* (car from)))
  (define boundary*
    (if (pre-boundary? from)
      (list (pre-boundary->boundary ty from))
      (blame-map-boundary* val (cdr from) (blame-compress-key* (car from)))))
  (void
    (let ((num-b (length boundary*)))
      (log-transient-info "blaming ~a boundar~a" num-b (if (= 1 num-b) "y" "ies")))
    (for ((b (in-list boundary*)))
      (log-transient-info "  ~s" (list (boundary-pos b) (boundary-neg b)))))
  (raise
    (exn:fail:contract:blame:transient
      (format
        "shape-check: value does not match expected type\n  value: ~s\n  type: ~s\n  lang: ~s\n  src: ~s"
        val (unquoted-printing-string (format "~a" ty)) 'typed/racket/shallow ctx)
      (current-continuation-marks)
      boundary*)))

(define (make-shallow-provide-contract pred ty-datum ctx)
  (define ((lnp blame) val neg-party)
    (if (eq? neg-party 'incomplete-blame-from-provide.rkt)
      #true
      (let ((pos-party (blame-positive blame)))
        (blame-map-set! val ty-datum (list 'boundary 'provide ctx pos-party neg-party)))))
  (make-flat-contract
    #:name (format "transient-projection:~a" (object-name pred))
    #:late-neg-projection lnp))

;; -----------------------------------------------------------------------------
;; --- blame map

(define THE-BLAME-MAP (make-hasheq))

(define blame-compress-key eq-hash-code)

(define (blame-compress-key* x)
  (if (list? x)
    (map blame-compress-key x)
    (list (blame-compress-key x))))

(define (pre-boundary? x)
  (and (pair? x) (eq? 'boundary (car x))))

;; make-blame-entry : (-> any/c (or/c symbol? (cons/c any/c symbol?)) blame-entry?)
(define (make-blame-entry ty-datum from)
  (if (pre-boundary? from)
    (pre-boundary->cast-info ty-datum from)
    (begin
      #;(printf "TTT make check-info ~s ~s~n" (eq-hash-code (car from)) (object-name (car from)))
      (check-info (cdr from) (eq-hash-code (car from)))))
  )

(define (cast-info->boundary ci)
  (define ty (cast-info-type ci))
  (define blame-val (cast-info-blame ci))
  (define pos-mod
    (let ((m (cadr blame-val)))
      (if (or (path? m)
              (and (pair? m) (path? (car m)))) ;; submod path
        m
        (with-handlers ((exn:fail:filesystem? (lambda (ex) m)))
          (let ((mpi (variable-reference->module-path-index (car blame-val))))
            (resolved-module-path-name (module-path-index-resolve (module-path-index-join m mpi))))
        ))))
  (make-boundary pos-mod (caddr blame-val) ty))

(define new-timestamp
  (let ((t (box 0)))
    (lambda () (begin0 (unbox t) (set-box! t (add1 (unbox t)))))))

(define (pre-boundary->cast-info ty-datum from)
  (ts-cast-info (cadr from) ty-datum (cddr from) (new-timestamp)))

(define (pre-boundary->boundary ty-datum from)
  (cast-info->boundary (pre-boundary->cast-info ty-datum from)))

(define (blame-map-ref v)
  (define entry# (hash-ref THE-BLAME-MAP (blame-compress-key v) (lambda () '#hash())))
  (map car (hash->list entry#)))

(define (blame-map-set! val ty-datum from)
  (unless (eq? val (eq-hash-code val))
    (define be (make-blame-entry ty-datum from))
    (hash-update! THE-BLAME-MAP (blame-compress-key val)
                  (lambda (curr) (blame-entry*-add curr be))
                  (lambda () (blame-entry*-init be)))))

(define (arg-cast val from)
  (blame-map-set! val #f from)
  val)

(define (blame-entry*-init v)
  (hash v 0))

(define (blame-entry*-add h v)
  (if (hash-has-key? h v)
    h
    (hash-set h v #true)))

(define (print-blame-map)
  (log-transient-info "blame map")
  (for (((k v) (in-hash THE-BLAME-MAP)))
    (log-transient-info " (~s (" k)
    (for ((vv (in-hash-keys v)))
      (log-transient-info "  ~s" vv))
    (log-transient-info " )"))
  (void))

(define (blame-map-boundary* val init-action key*)
  #;(printf "FIND BND ~s ~s ~s~n" val init-action key*)
  (define ts-cast*
    (let loop ([entry+path*
                 (apply append
                   (for/list ((key (in-list key*)))
                     (add-path* (blame-map-ref key) (list init-action))))])
      (apply append
       (for/list ((e+p (in-list entry+path*)))
         (define e (car e+p))
         (define curr-path (cdr e+p))
         (cond
           [(check-info? e)
            (define parent (check-info-parent e))
            (define action (blame-entry-from e))
            (define new-path
              (if (noop-action? action)
                curr-path
                (cons action curr-path)))
            (loop (add-path* (blame-map-ref parent) new-path))]
           [(cast-info? e)
            (define ty (cast-info-type e))
            (define blame-val (cast-info-blame e))
            (if (with-handlers ((exn:fail? (lambda (ex)
                                                  (printf "transient: internal error during value/type match~n value ~s~n type ~s~n message ~s~n" val ty (exn-message ex))
                                                  #f)))
                  (value-type-match? val ty curr-path (variable-reference->module-path-index (car blame-val))))
              '()
              (list e))]
           [else
             (raise-argument-error 'blame-map-boundary* "blame-entry?" e)])))))
  (map cast-info->boundary (sort ts-cast* > #:key ts-cast-info-time)))

(define (add-path* entry* path)
  (for/list ((e (in-list entry*)))
    (cons e path)))
