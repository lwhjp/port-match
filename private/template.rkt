#lang racket/base

(provide (all-defined-out))

(define (assert-input-port port)
  (unless (input-port? port)
    (raise-argument-error 'port-match "input-port?" port)))

(define (commit bytes port)
  (unless (zero? bytes)
    (port-commit-peeked bytes (port-progress-evt port) always-evt port)))

(define (no-match)
  (error 'port-match "no match"))
