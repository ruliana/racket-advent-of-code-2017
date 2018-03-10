#lang racket
(require "day-09-stream-parser.rkt" "day-09-stream-lexer.rkt" syntax/strip-context)

(provide (rename-out [stream-read-syntax read-syntax])
         get-info)

(define (stream-read-syntax path input)
  ; From input port and file path to AST
  (define parse-tree (parse path (make-tokenizer input path)))
  (strip-context
   #`(module day-09 "day-09-register-expander.rkt"
       #,parse-tree
       (display-max-value))))

(define (get-info port mod line col pos)
  (define (handle-query key default)
    (case key
      [(color-lexer)
       (dynamic-require "day-08-register-colorer.rkt" 'base-colorer)]
      [else default]))
  handle-query)