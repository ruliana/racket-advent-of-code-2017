#lang racket
(require "day-08-register-lexer.rkt"
         "day-08-register-parser.rkt"
         brag/support
         rackunit)

(define str #<<HERE
a inc 10
bc dec -10
x inc 1 if a > 10
HERE
  )

(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer str))
 '(program
   (line a inc 10)
   (line bc dec -10)
   (line x inc 1 if a > 10)))