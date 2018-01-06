#lang racket
(require "day-08-register-lexer.rkt"
         brag/support)

(displayln "BOOM!")

(provide base-colorer)

(define (base-colorer port)
  (define (handle-lexer-error exception)
    (define excn-srclocs (exn:fail:read-srclocs exception))
    (srcloc-token (token 'ERROR) (first excn-srclocs)))
  (define srcloc-tok
    (with-handlers ([exn:fail:read? handle-lexer-error])
      (base-lexer port)))
  (match srcloc-tok
    [(? eof-object?) (values srcloc-tok 'eof #f #f #f)]
    [else
     (match-define
       (srcloc-token
        (token-struct type val _ _ _ _ _)
        (srcloc _ _ _ posn span)) srcloc-tok)
     (define start posn)
     (define end (+ start span))
     (define cat
       (match (list type val)
         [(list 'NUMBER _) 'no-color]
         [(list 'IDENTIFIER 'if) 'constant]
         [(list 'IDENTIFIER 'dec) 'constant]
         [(list 'IDENTIFIER 'inc) 'constant]
         [(list 'IDENTIFIER _) 'symbol]
         [else 'no-color]))
     (values val cat #f start end)]))