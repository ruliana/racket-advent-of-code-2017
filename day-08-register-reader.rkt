#lang racket
(require syntax/readerr)

; === Boilerplate code
(provide (rename-out [regexp-read read]
                     [regexp-read-syntax read-syntax]))

(define (regexp-read in)
  (regexp-read-syntax #f in))

(define (regexp-read-syntax src in)
  (define commands (parse-commands src in))
  (datum->syntax
   #f
   `(module program racket
      (require "day-08-register.rkt")
      (inc "a" 2)
      (largest))))

(define (parse-commands src in)