#lang racket

(define (sqr x) (expt x 2))

(define (f x)
  (+ (* 4 (sqr x)) (* 4 x) 2))

(define (invf x)
  (add1 (* 1/2 (+ -1 (sqrt (sub1 x))))))

(define ((axis dir) layer)
  (+ (* 4 (sqr layer)) (* dir layer) 1))

(define north (axis -1))
(define east (axis -3))
(define south (axis 3))
(define west (axis 1))

(define (layer number)
  (floor (invf number)))

(define (direction number)
  (define angle (- (invf number) (layer number)))
  (cond
    [(<= angle 0.25) east]
    [(<= 0.25 angle 0.5) north]
    [(<= 0.5 angle 0.75) west]
    [(<= 0.75 angle) south]))

(define (steps-from number)
  (+ (layer number) (abs (- number ((direction number) (layer number))))))