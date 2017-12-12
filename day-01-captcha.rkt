#lang racket

(module+ test
  (require rackunit))

(define (digits string)
  (map string->number (regexp-match* #px"\\d" string)))

(define (captcher shifting numbers)
  (define shifted (sequence-tail (in-cycle numbers) shifting))
  (for/sum ([(current next) (in-parallel numbers shifted)]
            #:when (= current next))
    current))

(define (captcha numbers)
  (captcher 1 (digits numbers)))

(module+ test
  (check-equal? (captcha "1111") 4)
  (check-equal? (captcha "1122") 3)
  (check-equal? (captcha "1234") 0)
  (check-equal? (captcha "91212129") 9))

(define (captcha-half numbers)
  (define number-list (digits numbers))
  (define half-list (/ (length number-list) 2))
  (captcher half-list number-list))

(module+ test
  (check-equal? (captcha-half "1212") 6)
  (check-equal? (captcha-half "1221") 0)
  (check-equal? (captcha-half "123425") 4)
  (check-equal? (captcha-half "123123") 12)
  (check-equal? (captcha-half "12131415") 4))