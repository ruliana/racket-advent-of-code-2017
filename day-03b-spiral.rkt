#lang racket
(require racket/dict)

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

(define (fill grid stop-condition pointer direction)
  (define rslt (for/sum ([pos around]) (dict-ref grid (+ pointer pos) 0)))
  (define new-grid (dict-set grid pointer rslt))
  (define new-direction (if (dict-has-key? grid (+ pointer (sequence-ref direction 1)))
                            (sequence-tail direction 0)
                            direction))
  (displayln (sequence-ref new-direction 0))
  (if (stop-condition rslt)
      grid
      (fill new-grid stop-condition (+ pointer (sequence-ref new-direction 0)) new-direction)))

 (fill (hash 0+0i 1) (λ (x) true) 1+0i walk-order)