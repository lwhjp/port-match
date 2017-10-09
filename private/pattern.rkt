#lang racket/base

(provide (all-defined-out))

(require racket/syntax
         syntax/parse
         syntax/stx
         "expander.rkt")

(struct Pattern () #:transparent)
(struct Head-Pattern Pattern () #:transparent)
(struct Tail-Pattern Pattern () #:transparent)

(struct Any Pattern () #:transparent)
(struct And Pattern (pats) #:transparent)
(struct Or Pattern (pats) #:transparent)
(struct Not Pattern (pat) #:transparent)

(struct Seq Head-Pattern (pats) #:transparent)
(struct Eof Head-Pattern () #:transparent)
(struct Datum Head-Pattern (tail) #:transparent)

(struct Byte Datum () #:transparent)
(struct Bytes Datum (length) #:transparent)
(struct Char Datum () #:transparent)
(struct String Datum (length) #:transparent)
(struct Regexp Datum (pattern) #:transparent)

(struct App Tail-Pattern (proc tail) #:transparent)
(struct Pred Tail-Pattern (proc) #:transparent)
(struct Var Tail-Pattern (id) #:transparent)

(define (parse-pattern stx)
  ; TODO: should we be using syntax-rearm?
  (define-syntax-class head-pattern
    (pattern (~datum _) #:attr v (Any))
    (pattern ((~datum and) pat:head-pattern ...) #:attr v (And (attribute pat.v)))
    (pattern ((~datum or) pat:head-pattern ...) #:attr v (Or (attribute pat.v)))
    (pattern ((~datum not) pat:head-pattern) #:attr v (Not (attribute pat.v)))
    (pattern ((~datum seq) pat:head-pattern ...) #:attr v (Seq (attribute pat.v)))
    (pattern (~datum eof) #:attr v (Eof))
    (pattern ((~datum byte) pat:tail-pattern ...) #:attr v (Byte (And (attribute pat.v))))
    (pattern ((~datum bytes) length:expr pat:tail-pattern ...) #:attr v (Bytes (And (attribute pat.v)) #'length))
    (pattern ((~datum char) pat:tail-pattern ...) #:attr v (Char (And (attribute pat.v))))
    (pattern ((~datum string) length:expr pat:tail-pattern ...) #:attr v (String (And (attribute pat.v)) #'length))
    (pattern ((~datum regexp) pattern:expr pat:tail-pattern ...) #:attr v (Regexp (And (attribute pat.v)) #'pattern)))
  (define-syntax-class tail-pattern
    (pattern (~datum _) #:attr v (Any))
    (pattern ((~datum and) pat:tail-pattern ...) #:attr v (And (attribute pat.v)))
    (pattern ((~datum or) pat:tail-pattern ...) #:attr v (Or (attribute pat.v)))
    (pattern ((~datum not) pat:tail-pattern) #:attr v (Not (attribute pat.v)))
    (pattern ((~datum app) proc:expr pat:tail-pattern) #:attr v (App #'proc (attribute pat.v)))
    (pattern ((~datum ?) proc:expr pat:tail-pattern ...) #:attr v (And (cons (Pred #'proc) (attribute pat.v))))
    (pattern id:id #:attr v (Var #'id)))
  (syntax-parse stx
    [(expander:id arg ...) #:when (syntax-local-value/record #'expander port-match-expander?)
     (parse-pattern ((port-match-expander (syntax-local-value #'expander)) stx))]
    [pat:head-pattern (attribute pat.v)]))

(define (pattern-vars pat)
  (cond
    [(And? pat) (apply append (map pattern-vars (And-pats pat)))]
    [(Or? pat) (apply append (map pattern-vars (Or-pats pat)))]
    [(Not? pat) (pattern-vars (Not-pat pat))]
    [(App? pat) (pattern-vars (App-tail pat))]
    [(Var? pat) (list (Var-id pat))]
    [(Seq? pat) (apply append (map pattern-vars (Seq-pats pat)))]
    [(Datum? pat) (pattern-vars (Datum-tail pat))]
    [else '()]))
