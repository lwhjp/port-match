#lang racket/base

(provide
 define-port-match-expander
 port-match)

(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "compile.rkt"
                     "expander.rkt"
                     "pattern.rkt")
         racket/stxparam
         "template.rkt")

(define-syntax (define-port-match-expander stx)
  (syntax-parse stx
    [(_ id:id exp-stx:expr)
     #'(define-syntax id
         (let ()
           (struct expander ()
             #:property prop:port-match-expander exp-stx)
           (expander)))]))

(define-syntax (port-match stx)
  (syntax-parse stx
    [(_ in:expr) #'(begin (assert-input-port in) (no-match))]
    [(_ in:expr (pat body:expr ...+) ...)
     (with-syntax ([(pat-id ...) (generate-temporaries #'(pat ...))])
       #`(let ([in-port in])
           (assert-input-port in-port)
           (letrec
               #,(stx-map
                  (λ (pat-id pat-stx rhs-stx fail-stx)
                    #`[#,pat-id
                       (lambda () #,(compile-clause pat-stx rhs-stx #'in-port fail-stx))])
                  #'(pat-id ...)
                  #'(pat ...)
                  #'((body ...) ...)
                  (stx-cdr #'((pat-id) ... (no-match))))
             (#,(stx-car #'(pat-id ...))))))]))
