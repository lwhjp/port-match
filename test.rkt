#lang racket/base

(require (for-syntax racket/base)
         rackunit
         "main.rkt")

(define-syntax (convert-syntax-error stx)
  (syntax-case stx ()
    [(_ expr)
     (with-handlers ([exn:fail:syntax? (λ (e) #`(error '#,(exn-message e)))])
       (parameterize ([error-print-source-location #f])
         (local-expand #'expr 'expression '())))]))

(define oib open-input-bytes)
(define ois open-input-string)

(check-exn exn:fail? (λ () (port-match (oib #""))))

(check-equal? (port-match (oib #"\7") [(byte b) b]) 7)
(check-equal? (port-match (oib #"\3\4\5\6") [(bytes 3 bstr) bstr]) #"\3\4\5")
(check-equal? (port-match (ois "テスト") [(char c) c]) #\テ)
(check-equal? (port-match (ois "テスト") [(string 2 s) s]) "テス")
(check-equal? (port-match (ois "abcd")
                [(regexp #rx".c") 'bad]
                [(seq (char) (regexp #rx".c")) 'good])
              'good)
(check-equal? (port-match (ois "abcd")
                [(or (regexp #rx"z" v) (char v)) v])
              #\a)

(check-exn exn:fail? (λ () (port-match (oib #"") [(byte b) b])))
(check-equal? (port-match (oib #"\3\4") [(bytes 3 bstr) bstr] [(byte b) b]) 3)

(check-equal? (port-match (ois "花\3") [(seq (char c) (byte b)) (cons b c)]) (cons 3 #\花))
(check-equal? (port-match (oib #"") [eof 'ok]) 'ok)

(check-equal? (port-match (oib #"\2\4\6")
                [(and (byte b) (bytes 2 bstr)) (cons b bstr)])
              (cons 2 #"\2\4"))

(check-equal? (port-match (oib #"\2\4\6")
                [(or (bytes 5 x) (byte x)) x])
              2)

(check-equal? (port-match (ois "abc") [(not eof) 'ok]) 'ok)

(check-equal? (port-match (ois " ") [(char (app char->integer v)) v]) 32)

(check-equal? (port-match (ois "def")
                [(char (? char-numeric?)) 'bad]
                [(char (? char-alphabetic?)) 'good])
              'good)

(check-exn exn:fail? (λ () (convert-syntax-error (port-match (oib #"x") [(or (byte a) (byte b)) 'dummy]))))
