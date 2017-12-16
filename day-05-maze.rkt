#lang racket
(define (update-jump! maze [pos 0] [steps 0])
  (define (jump maze pos steps)
    (define jumper (vector-ref maze pos))
    (vector-set! maze pos (add1 jumper))
    (update-jump! maze (+ pos jumper) (add1 steps)))
  (if (>= pos (vector-length maze))
      steps
      (jump maze pos steps)))

(module+ test
  (require rackunit)
  (define maze (vector 0 3 0 1 -3))
  (define steps (update-jump! maze))
  (check-equal? steps 5)
  (check-equal? maze #(2 5 0 1 -2)))

(define (count-steps input)
  (update-jump!
   (for/vector ([line (in-lines input)])
     (string->number line))))

(printf "Steps: ~a\n"
        (call-with-input-file "day-05-input.txt" count-steps #:mode 'text))