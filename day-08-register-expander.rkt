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
(define biggest -inf.0)

(define (qry register)
  (dict-ref memory register 0))

(define (upd register value)
  (dict-set! memory register value))

(define (upd-biggest value)
  (set! biggest (max biggest value)))

(define (inc register amount)
  (let* ([old-value (qry register)]
         [new-value (+ amount old-value)])
    (upd register new-value)
    (upd-biggest new-value)))

(define (dec register amount)
  (inc register (- amount)))

(define (display-memory)
  (displayln memory))

(define (display-max-value)
  (printf "Max value in registers: ~a\n" (argmax identity (dict-values memory)))
  (printf "     Peak in registers: ~a\n" (inexact->exact biggest)))

(define != (negate =))