#lang racket
(require syntax/readerr)

; === Boilerplate code
(provide (rename-out [regexp-read read]
                     [regexp-read-syntax read-syntax]))

(define (regexp-read in)
  (regexp-read-syntax #f in))

; === Call parse and emit module
(define (regexp-read-syntax src in)
  (define commands (parse-commands src in))
  #`(module program racket
      (require "day-08-register.rkt")
      #,@commands
      (largest)))

; === Parse and build commands
(define (parse-commands src in)
  (define matches (regexp-match* #px"\\s*(\\w+)\\s+(inc|dec)\\s+(\\d+)"
                                 in
                                 #:match-select rest))
  (map translate matches))

(define (translate match)
  (match-define (list reg comm num) match)
  `(,(->symbol comm)
    ,(->string reg)
    ,(->number num)))
  
; === Helpers
(define ->string bytes->string/utf-8)
         
(define (->symbol byte-string)
  (string->symbol (->string byte-string)))

(define (->number byte-string)
  (string->number (->string byte-string)))