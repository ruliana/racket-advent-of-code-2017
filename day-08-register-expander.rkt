#lang racket
(require racket/dict (for-syntax syntax/parse))

(provide program line ; Macros (Transformers)
         inc dec qry != display-memory display-max-value ; Our functions
         > < >= <= ; Expose racket functions
         (rename-out [= ==])
         #%app #%top #%datum #%top-interaction #%module-begin) ; Racket base

(define-syntax (program stx)
  (syntax-case stx ()
    [(_ lines ...) #'(begin lines ...)]))

(define-syntax (line stx)
  (syntax-parse stx
    #:datum-literals (if)
    [(_ register command amount if other-reg comparator other-amt)
     #'(when (comparator (qry 'other-reg) other-amt) (command 'register amount))]
    [(_ register command amount) #`(command 'register amount)]))

(define memory (make-hash))

(define (qry register)
  (dict-ref memory register 0))

(define (inc register amount)
  (dict-update! memory register (λ (x) (+ x amount)) 0))

(define (dec register amount)
  (dict-update! memory register (λ (x) (- x amount)) 0))

(define (display-memory)
  (displayln memory))

(define (display-max-value)
  (printf "Max value in registers: ~a\n" (argmax identity (dict-values memory))))

(define != (negate =))