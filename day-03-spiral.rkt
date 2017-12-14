#lang racket

; This program uses a "closed form" approach,
; meaning it does not iterate to find the
; solution, instead it uses a mathematical
; formula to reach the same result.

; Simple x^2 for convenience
(define (sqr x) (expt x 2))

; "Polar" coordinates for the spiral.
; The integer part is the distantce from
; the origin square (1). The fractional
; part is turn around the spiral, starting
; from bottom-right.
; It's used to locate the number without
; having to fill the spiral.
(define (coordinates-of x)
  (add1 (* 1/2 (sub1 (sqrt (sub1 x))))))

; Given a direction, it returns a function
; that given a layer (distance), returns
; the number straight on that direction.
; It's used calculate the "turning point"
; for the manhattan walker.
(define ((axis dir) layer)
  (+ (* 4 (sqr layer)) (* dir layer) 1))

; Returns the number on the given layer
; straight on that direction.
(define north (axis -1))
(define east (axis -3))
(define south (axis 3))
(define west (axis 1))

; Given a number, what's its layer?
; It's the distance on my "polar"
; coordinates.
(define (layer number)
  (floor (coordinates-of number)))

; Given a number, returns a function
; for the direction it is.
(define (direction number)
  (define angle (- (coordinates-of number) (layer number)))
  (cond
    [(<= angle 0.25) east]
    [(<= 0.25 angle 0.5) north]
    [(<= 0.5 angle 0.75) west]
    [(<= 0.75 angle) south]))

; Calculate how many steps in a
; manhattan walker I need to reach
; the number.
(define (steps-from number)
  (define deep (layer number))
  (define straight-number ((direction number) deep))
  (define lateral-move (abs (- number straight-number)))
  (inexact->exact (+ deep lateral-move)))

(module+ test
  (require rackunit)
  (check-equal? (steps-from 1) 0)
  (check-equal? (steps-from 2) 1)
  (check-equal? (steps-from 3) 2)
  (check-equal? (steps-from 4) 1)
  (check-equal? (steps-from 5) 2)
  (check-equal? (steps-from 6) 1)
  (check-equal? (steps-from 7) 2)
  (check-equal? (steps-from 8) 1)
  (check-equal? (steps-from 9) 2)
  (check-equal? (steps-from 126) 7)
  (check-equal? (steps-from 127) 6)
  (check-equal? (steps-from 128) 7)
  (check-equal? (steps-from 325489) 552))