#lang racket

(define (parse line)
  (list->vector (map string->number (string-split line))))

(define (distribute bank)
  (define size (vector-length bank))
  (define top (vector-argmax identity bank))
  (define start (vector-member top bank))
  (define-values (quot rem) (quotient/remainder top size))
  (for/vector ([(value index) (in-indexed bank)])
    (cond
      [(<= index (- (+ start rem) size)) (+ value quot 1)]
      [(< start index (+ start rem 1)) (+ value quot 1)]
      [(= start index) quot]
      [else (+ value quot)])))

(define (reallocate bank [counter 0] [visited (hash)])
  (if (dict-has-key? visited bank)
      (values counter (- counter (dict-ref visited bank)))
      (reallocate (distribute bank)
                  (add1 counter)
                  (dict-set visited bank counter))))

(module+ test
  (require rackunit)
  (define-values (loop-at cycle-at) (reallocate #(0 2 7 0)))
  (check-equal? loop-at 5)
  (check-equal? cycle-at 4))

(define (print-result input)
  (define line (sequence-ref (in-lines input) 0))
  (define-values (loop-at cycle-at) (reallocate (parse line)))
  (printf "Puzzle A: ~a\n" loop-at)
  (printf "Puzzle B: ~a\n" cycle-at))

(call-with-input-file "day-06-input.txt" print-result #:mode 'text)


