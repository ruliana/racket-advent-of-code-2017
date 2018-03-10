#lang racket
(require brag/support)
(module+ test
  (require rackunit))

(provide make-tokenizer
         base-lexer)

; Convenience definitions
(define-lex-abbrev integer (:: (:? "-") (:+ numeric)))
(define-lex-abbrev identifier (:+ (:or "!" alphabetic symbolic)))

; Heavy lifting: from text to tokens
(define base-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   ["\n" (token 'NEWLINE)]
   [whitespace (token 'SPACE #:skip? #t)]
   [integer (token 'NUMBER (string->number lexeme))]
   [identifier (token 'IDENTIFIER (string->symbol lexeme))]))

(module+ test
  (define-match-expander tokens
    (syntax-rules ()
      [(_ (t v) ...) (list (srcloc-token (token-struct t v _ _ _ _ _) _) ...)]))
  (define (lex str)
    (apply-lexer base-lexer str))
  (check-equal? (lex "") empty)
  (check-match (lex "123") (tokens ('NUMBER 123)))
  (check-match (lex "-321") (tokens ('NUMBER -321)))
  (check-match (lex "ab") (tokens ('IDENTIFIER 'ab)))
  (check-match (lex "a+b") (tokens ('IDENTIFIER 'a+b)))
  (check-match (lex "ab -12 cd") (tokens ('IDENTIFIER 'ab)
                                         ('SPACE #f)
                                         ('NUMBER -12)
                                         ('SPACE #f)
                                         ('IDENTIFIER 'cd))))

; Boilerplate code (pretty much the same for other languages)
(define (make-tokenizer input [path #f])
  (port-count-lines! input)
  (lexer-file-path path)
  (define (next-token) (base-lexer input))
  next-token)