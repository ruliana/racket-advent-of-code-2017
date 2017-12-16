#lang racket
(require threading)

(define preprocess (make-parameter identity))

(define (char-sort text)
  (~> text
      string->list
      (sort char<?)
      list->string))      

(define (valid? text)
  (let loop ([tokens (string-split text)]
             [visited (set)])
    (match tokens
      ['() #t]
      [(list head tail ...) #:when (set-member? visited ((preprocess) head)) #f]
      [(list head tail ...) (loop tail (set-add visited ((preprocess) head)))])))

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

(printf "Puzzle A: ~a\n"
 (call-with-input-file "day-04-input.txt" count-valid #:mode 'text))

(parameterize ([preprocess char-sort])
  (printf "Puzzle B: ~a\n"
   (call-with-input-file "day-04-input.txt" count-valid #:mode 'text)))