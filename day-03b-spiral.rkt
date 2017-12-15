#lang racket
(require racket/dict
         racket/generator)

; Convenience functions
(define (first seq) (sequence-ref seq 0))
(define (second seq) (sequence-ref seq 1))
(define (tail seq) (sequence-tail seq 1))

; Definitions
(define right 1)
(define up 0+1i)
(define left -1)
(define down 0-1i)
(define around (list right
                     up
                     left
                     down
                     (+ right up)
                     (+ up left)
                     (+ left down)
                     (+ down right)))                      

(define walk-order (in-cycle (list right up left down)))

(define (spiral-sum)
  (in-generator
   #:arity 1
   (yield 1)
   (let loop ([grid (hash 0+0i 1)]
              [pointer 0+0i]
              [direction walk-order])
     (define look-ahead (+ pointer (second direction)))
     (define new-direction (if (dict-has-key? grid look-ahead)
                               direction
                               (tail direction)))
     (define new-pointer (+ pointer (first new-direction)))
     (define rslt (for/sum ([pos around]) (dict-ref grid (+ new-pointer pos) 0)))
     (define new-grid (dict-set grid new-pointer rslt))
     (yield rslt)
     (loop new-grid new-pointer new-direction))))

(module+ test
  (require rackunit)
  (let ([rslt (stop-after (spiral-sum) (λ (x) (>= x 806)))])
    (check-equal? (sequence->list rslt)
                  '(1 1 2 4 5 10 11 23 25 26 54 57 59 122 133 142 147 304 330 351 362 747 806))))