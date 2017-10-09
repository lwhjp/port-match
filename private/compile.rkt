#lang racket/base

(provide
 compile-clause)

(require (for-template racket/base
                       racket/port
                       "template.rkt")
         racket/match
         racket/syntax
         syntax/stx
         "pattern.rkt")

(define (compile-clause pat-stx rhs-stx in-id fail-stx)
  (define pat (parse-pattern pat-stx))
  (with-syntax ([peek-in (gensym 'peek)])
    #`(let ([peek-in (peeking-input-port #,in-id)])
        #,(compile-head
           pat
           (λ (vars)
             (with-syntax ([((var-id . var-src) ...) vars])
               #`(let ([var-id var-src] ...)
                   (commit (file-position peek-in) #,in-id)
                   #,@rhs-stx)))
           '()
           #'peek-in
           fail-stx))))

(define (compile-head pat rest vars in-id fail-stx)
  (match pat
    [(Seq pats)
     (let next-pat ([pats pats]
                    [vars vars])
       (if (null? pats)
           (rest vars)
           (compile-head (car pats)
                         (λ (vars)
                           (next-pat (cdr pats) vars))
                         vars
                         in-id
                         fail-stx)))]
    [(Eof) #`(if (eof-object? (peek-byte #,in-id)) #,(rest vars) #,fail-stx)]
    [(Datum tail)
     (with-syntax ([datum (gensym 'datum)])
       (compile-datum pat
                      #'datum
                      in-id
                      (compile-tail tail rest #'datum vars in-id fail-stx)
                      fail-stx))]
    [_
     (compile-common pat
                     rest
                     compile-head
                     vars
                     in-id
                     fail-stx)]))

(define (compile-datum pat datum-id in-id rest-stx fail-stx)
  (with-syntax ([datum datum-id]
                [in in-id]
                [rest rest-stx]
                [fail fail-stx])
    (match pat
      [(Byte _) #'(let ([datum (read-byte in)]) (if (eof-object? datum) fail rest))]
      [(Bytes _ length-stx)
       #`(let* ([length #,length-stx] [datum (read-bytes length in)])
           (if (or (eof-object? datum) (< (bytes-length datum) length)) fail rest))]
      [(Char _) #'(let ([datum (read-char in)]) (if (eof-object? datum) fail rest))]
      [(String _ length-stx)
       #`(let* ([length #,length-stx] [datum (read-string length in)])
           (if (or (eof-object? datum) (< (string-length datum) length)) fail rest))])))

(define (compile-tail pat rest datum-id vars in-id fail-stx)
  (match pat
    [(App proc-stx tail)
     (with-syntax ([orig-datum datum-id]
                   [datum (gensym 'datum)]
                   [proc proc-stx])
       #`(let-values ([(datum) (proc orig-datum)])
           #,(compile-tail tail rest #'datum vars in-id fail-stx)))]
    [(Pred proc-stx) #`(if (#,proc-stx #,datum-id) #,(rest vars) #,fail-stx)]
    [(Var id) (rest (cons (cons id datum-id) vars))]
    [_
     (compile-common pat
                     rest
                     (λ (pat rest vars in-id fail-stx)
                       (compile-tail pat rest datum-id vars in-id fail-stx))
                     vars
                     in-id
                     fail-stx)]))

(define (compile-common pat rest pat-compile vars in-id fail-stx)
  (match pat
    [(Any) (rest vars)]
    [(And '()) (rest vars)]
    [(And (list pat)) (pat-compile pat rest vars in-id fail-stx)]
    [(And pats)
     (let next-pat ([pats pats]
                    [vars vars]
                    [peek-ids '()])
       (if (null? pats)
           #`(begin
               (commit (apply max (map file-position (list #,@peek-ids))) #,in-id)
               #,(rest vars))
           (with-syntax ([peek-in (gensym 'peek)])
             #`(let ([peek-in (peeking-input-port #,in-id)])
                 #,(pat-compile (car pats)
                                (λ (vars) (next-pat (cdr pats) vars (cons #'peek-in peek-ids)))
                                vars
                                #'peek-in
                                fail-stx)))))]
    [(Or '()) fail-stx]
    [(Or (list pat)) (pat-compile pat rest vars in-id fail-stx)]
    [(Or pats)
     (let ([sorted-pat-vars (map (λ (pat) (sort (pattern-vars pat) symbol<? #:key syntax-e)) pats)])
       (define vars (car sorted-pat-vars))
       (unless (andmap (λ (pat-vars)
                         (and (eqv? (length vars) (length pat-vars))
                              (andmap free-identifier=? vars pat-vars)))
                       (cdr sorted-pat-vars))
         (raise-syntax-error 'port-match "different variables bound in (or ...) patterns")))
     (with-syntax* ([(pat-id ...) (generate-temporaries pats)]
                    [(fail-stx ...) (stx-cdr #`((pat-id) ... #,fail-stx))])
       #`(letrec
             #,(for/list ([pat pats]
                          [pat-id (syntax->list #'(pat-id ...))]
                          [fail-stx (syntax->list #'(fail-stx ...))])
                 (with-syntax ([peek-in (gensym 'peek)])
                   #`[#,pat-id
                      (lambda ()
                        (let ([peek-in (peeking-input-port #,in-id)])
                          #,(pat-compile pat
                                         (λ (vars)
                                           #`(begin
                                               (commit (file-position peek-in) #,in-id)
                                               #,(rest vars)))
                                         vars
                                         #'peek-in
                                         fail-stx)))]))
           (#,(stx-car #'(pat-id ...)))))]
    [(Not pat)
     (pat-compile pat
                  (λ (vars) fail-stx)
                  vars
                  in-id
                  (rest vars))]
    [_ (error "BUG: invalid pattern")]))
