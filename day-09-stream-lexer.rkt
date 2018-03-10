#lang racket
(require brag/support)

(module+ test
  (require rackunit))

(provide make-tokenizer lexer)

(define-lex-abbrev escape (:: "!" any-char))

(define base-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   [escape (token 'ESCAPE #:skip? #t)]
   ["{" (token 'OPEN-GROUP lexeme)]
   ["}" (token 'CLOSE-GROUP lexeme)]
   ["<" (token 'OPEN-GARBAGE lexeme)]
   [">" (token 'CLOSE-GARBAGE lexeme)]
   [any-char (token 'CHAR lexeme)]))

(module+ test
  (define-match-expander tokens
    (syntax-rules ()
        [(_ t ...) (list (srcloc-token (token-struct t _ _ _ _ _ _) _) ...)]))
  (define (lex str) (apply-lexer base-lexer str))
  (check-equal? (lex "") empty)
  (check-match (lex "<>") (tokens 'OPEN-GARBAGE 'CLOSE-GARBAGE))
  (check-match (lex "<a>") (tokens 'OPEN-GARBAGE 'CHAR 'CLOSE-GARBAGE))
  (check-match (lex "<!>>") (tokens 'OPEN-GARBAGE 'ESCAPE 'CLOSE-GARBAGE))
  (check-match (lex "a{!{}}") (tokens 'CHAR 'OPEN-GROUP 'ESCAPE 'CLOSE-GROUP 'CLOSE-GROUP))
  (check-match (lex "<<>") (tokens 'OPEN-GARBAGE 'OPEN-GARBAGE 'CLOSE-GARBAGE)))

; Boilerplate code (pretty much the same for other languages)
(define (make-tokenizer input [path #f])
  (port-count-lines! input)
  (lexer-file-path path)
  (define (next-token) (base-lexer input))
  next-token)