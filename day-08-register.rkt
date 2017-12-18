#lang racket
; === Santa's Computer Interface

(provide inc dec largest)

(define registers (make-hash))

(define (inc reg number)
  (dict-update! registers reg (curry + number) 0))

(define (dec reg number)
  (inc reg (- number)))

(define (largest)
  (apply max (dict-values registers)))