#lang racket
(require brag/support)
(module+ test
  (require rackunit))

(provide make-tokenizer)

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
  (define (lex str)
    (apply-lexer base-lexer str))
  (check-equal? (lex "") empty)
  (check-equal? (lex "123") (list (srcloc-token (token 'NUMBER 123)
                                                (srcloc 'string #f #f 1 3))))
  (check-equal? (lex "-321") (list (srcloc-token (token 'NUMBER -321)
                                                 (srcloc 'string #f #f 1 4))))
  (check-equal? (lex "ab") (list (srcloc-token (token 'IDENTIFIER 'ab)
                                               (srcloc 'string #f #f 1 2))))
  (check-equal? (lex "a+b") (list (srcloc-token (token 'IDENTIFIER 'a+b)
                                                (srcloc 'string #f #f 1 3))))
  (check-equal? (lex "ab -12 cd") (list (srcloc-token
                                         (token-struct 'IDENTIFIER 'ab #f #f #f #f #f)
                                         (srcloc 'string #f #f 1 2))
                                        (srcloc-token
                                         (token-struct 'SPACE #f #f #f #f #f #t)
                                         (srcloc 'string #f #f 3 1))
                                        (srcloc-token
                                         (token-struct 'NUMBER -12 #f #f #f #f #f)
                                         (srcloc 'string #f #f 4 3))
                                        (srcloc-token
                                         (token-struct 'SPACE #f #f #f #f #f #t)
                                         (srcloc 'string #f #f 7 1))
                                        (srcloc-token
                                         (token-struct 'IDENTIFIER 'cd #f #f #f #f #f)
                                         (srcloc 'string #f #f 8 2)))))

; Boilerplate code (pretty much the same for other languages)
(define (make-tokenizer input [path #f])
  (port-count-lines! input)
  (lexer-file-path path)
  (define (next-token) (base-lexer input))
  next-token)