#lang racket

(define puzzle-a add1)
(define (puzzle-b n) (if (<= 3 n) (sub1 n) (add1 n))) 

(define update-jumper (make-parameter puzzle-a))

(define (walk-through maze)
  (define move (update-jumper))
  (define current (vector-copy maze))
  (define end-point (vector-length current))
  (define (stop-condition pos steps)
    (if (<= end-point pos)
        (values steps current)
        (jump pos steps)))
  (define (jump pos steps)
    (define jumper (vector-ref current pos))
    (vector-set! current pos (move jumper))
    (stop-condition (+ pos jumper) (add1 steps)))
  (stop-condition 0 0))

(module+ test
  (require rackunit)
  (define maze (vector 0 3 0 1 -3))
  (parameterize ([update-jumper puzzle-a])
    (define-values (steps current) (walk-through maze))
    (check-equal? steps 5)  
    (check-equal? current #(2 5 0 1 -2)))
  (parameterize ([update-jumper puzzle-b])
    (define-values (steps current) (walk-through maze))
    (check-equal? steps 10)
    (check-equal? current #(2 3 2 3 -1))))

(define (count-steps input)
  (let-values ([(steps _)
                (walk-through
                 (for/vector ([line (in-lines input)])
                   (string->number line)))])
    steps))

(parameterize ([update-jumper puzzle-a])
  (printf "Puzzle A: ~a\n"
          (call-with-input-file "day-05-input.txt" count-steps #:mode 'text)))

(parameterize ([update-jumper puzzle-b])
  (printf "Puzzle B: ~a\n"
          (call-with-input-file "day-05-input.txt" count-steps #:mode 'text)))