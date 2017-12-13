#lang racket

(define (sqr x) (expt x 2))

(define (f x)
  (+ (* 4 (sqr x)) (* 4 x) 2))

(define (invf x)
  (add1 (* 1/2 (+ -1 (sqrt (sub1 x))))))

(define ((axis dir) level)
  (+ (* 4 (sqr level)) (* dir level) 1))

(define north (axis -1))
(define east (axis -3))
(define south (axis 3))
(define west (axis 1))

(define (level number)
  (floor (invf number)))

(define (direction number)
  (define angle (- (invf number) (level number)))
  (cond
    [(<= 0 angle 0.25) east]
    [(<= 0.25 angle 0.5) north]
    [(<= 0.5 angle 0.75) west]
    [(<= 0.75 angle) south]))