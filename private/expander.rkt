#lang racket/base

(provide (all-defined-out))

(define-values (prop:port-match-expander port-match-expander? port-match-expander)
  (make-struct-type-property 'port-match-expander))
