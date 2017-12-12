#lang racket
(module+ test
  (require rackunit))

(define (lmax lst) (apply max lst))
(define (lmin lst) (apply min lst))

(define (parse text)
  (for/list ([line (in-lines (open-input-string text))])
    (map string->number (regexp-split #px"\\s+" line))))

(define (checksum spreadsheet)
  (for*/sum ([line spreadsheet])
    (- (lmax line) (lmin line))))

(define (divisibles-on-line line)
  (define all-pairs (sequence-map (λ (e) (list (lmax e) (lmin e)))
                                  (in-combinations line 2)))
  (for/list ([elems all-pairs]
             #:when (zero? (remainder (first elems) (second elems))))
    (quotient (first elems) (second elems))))

(module+ test
  (check-equal? (divisibles-on-line '(5 9 2 8)) '(4))
  (check-equal? (divisibles-on-line '(9 4 7 3)) '(3))
  (check-equal? (divisibles-on-line '(3 8 6 5)) '(2)))

(define (divisibles-on-spreadsheet spreadsheet)
  (for*/sum ([line spreadsheet])
    (apply + (divisibles-on-line line))))

(module+ test
  (check-equal? (checksum '((5 1 9 5)
                            (7 5 3)
                            (2 4 6 8)))
                18))
