#lang racket
(require "day-08-register-parser.rkt" "day-08-register-lexer.rkt" syntax/strip-context)

(provide (rename-out [santa-read-syntax read-syntax])
         get-info)

(define (santa-read-syntax path input)
  ; From input port and file path to AST
  (define parse-tree (parse path (make-tokenizer input path)))
  (strip-context
   #`(module day-08 "day-08-register-expander.rkt"
       #,parse-tree
       (display-max-value))))

(define (get-info port mod line col pos)
  (define (handle-query key default)
    (case key
      [(color-lexer)
       (dynamic-require "day-08-register-colorer.rkt" 'base-colorer)]
      [else default]))
  handle-query)