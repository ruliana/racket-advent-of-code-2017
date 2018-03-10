#lang racket
(require racket/dict (for-syntax syntax/parse))

(provide stream group garbage ; Macros (Transformers)
         ;display-score ; Our functions
         + ; Expose racket functions
         #%app #%top #%datum #%top-interaction #%module-begin) ; Racket base

(define-syntax (stream stx)
  (syntax-case stx ()
    [(_ first rest ...) #'(+ first rest ...)]))

(define-syntax (group stx)
  (syntax-case stx ()
    [(_ _ rest ... _) #'(+ 1 rest ...)]))

(define-syntax (garbage stx)
  (syntax-case stx ()
    [(_ rest ...) #'0]))

(stream (group "{" 
               (group "{" "}")
               "}")
        (group "{" "}"))
