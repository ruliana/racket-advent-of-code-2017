#lang racket

(define (valid? text)
  (let loop ([tokens (string-split text)]
             [visited (set)])
    (match tokens
      ['() #t]
      [(list head tail ...) #:when (set-member? visited head) #f]
      [(list head tail ...) (loop tail (set-add visited head))])))

(module+ test
  (require rackunit)
  (check-true (valid? ""))
  (check-true (valid? "aa"))
  (check-false (valid? "aa aa"))
  (check-true (valid? "aa bb cc dd ee"))
  (check-false (valid? "aa bb cc dd aa"))
  (check-true (valid? "aa bb cc dd aaa")))

(define (count-valid input)
  (for/sum ([line (in-lines input)])
    (if (valid? line) 1 0)))

(displayln
 (call-with-input-file "day-04-password.txt" count-valid #:mode 'text))