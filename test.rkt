#lang racket/base

(require rackunit
         "main.rkt")

(define oib open-input-bytes)
(define ois open-input-string)

(check-exn exn:fail? (λ () (port-match (oib #""))))

(check-equal? (port-match (oib #"\7") [(byte b) b]) 7)
(check-equal? (port-match (oib #"\3\4\5\6") [(bytes 3 bstr) bstr]) #"\3\4\5")
(check-equal? (port-match (ois "テスト") [(char c) c]) #\テ)
(check-equal? (port-match (ois "テスト") [(string 2 s) s]) "テス")

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