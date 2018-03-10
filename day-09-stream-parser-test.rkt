#lang racket
(require "day-09-stream-lexer.rkt"
         "day-09-stream-parser.rkt"
         brag/support
         rackunit)

(define (parse str)
  (parse-to-datum (apply-tokenizer make-tokenizer str)))

(define (count-groups str)
  (let loop ([tree (parse str)])
    (match tree
      ['group 1]
      [(list first rest ...) (+ (loop first) (loop rest))]
      [_ 0])))

(define (score str)
  (let loop ([tree (parse str)]
             [value 1])
    (match tree
      [(list 'group inner ...) (+ value (loop inner (add1 value)))]
      [(list other rest ...) (+ (loop other value)
                                (loop rest value))]
      [_ 0])))

; All Garbage
(define (check-garbage? str)
  (check-equal? (parse str) '(stream (garbage "<" ">"))))
  
(check-garbage? "<>")
(check-garbage? "<random characters>")
(check-garbage? "<<<<>")
(check-garbage? "<{!>}>")
(check-garbage? "<!!>")
(check-garbage? "<!!!>>")
(check-garbage? "<{o\"i!a,<{i<a>")

; Groups!
(define (check-groups? str num)
  (check-equal? (count-groups str) num))

(check-groups? "{}" 1)
(check-groups? "{{{}}}" 3)
(check-groups? "{{},{}}" 3)
(check-groups? "{{{},{},{{}}}}" 6)
(check-groups? "{<{},{},{{}}>}" 1)
(check-groups? "{<a>,<a>,<a>,<a>}" 1)
(check-groups? "{{<a>},{<a>},{<a>},{<a>}}" 5)
(check-groups? "{{<!>},{<!>},{<!>},{<a>}}" 2)

; Score
(define (check-score? str expected-score)
  (check-equal? (score str) expected-score))

(check-score? "{}" 1)
(check-score? "{{{}}}" 6)
(check-score? "{{},{}}" 5)
(check-score? "{{{},{},{{}}}}" 16)
(check-score? "{<{},{},{{}}>}" 1)
(check-score? "{<a>,<a>,<a>,<a>}" 1)
(check-score? "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9)
(check-score? "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9)
(check-score? "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3)