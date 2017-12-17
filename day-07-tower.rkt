#lang racket

(define ((length=? x) y) (= x (length y)))

(struct program (name weight children) #:mutable)

(define (program-shoulder prog)
  (+ (program-weight prog)
     (for/sum ([c (program-children prog)])
       (program-shoulder c))))

(define (index->tower tower-index)
  (define index (hash-copy tower-index))
  (define (ref e) (dict-ref tower-index e))
  (define (remove! e) (dict-remove! index e))
  (for ([current (in-dict-values tower-index)])
    (define children (program-children current))
    (set-program-children! current (map ref children))
    (for-each remove! children))
  (first (dict-values index)))

(define (find-unbalanced root [siblings '()])
  (define groups (group-by program-shoulder (program-children root)))
  (define-values (different same) (partition (length=? 1) groups))
  (if (empty? different)
      (values root siblings)
      (find-unbalanced (caar different) (flatten same))))

(define (rebalance root)
  (match-define-values (unbalanced (list balanced others ...))
                       (find-unbalanced root))
  (+ (program-weight unbalanced)
     (- (program-shoulder balanced)
        (program-shoulder unbalanced))))

(module+ test
  (require rackunit)
  (define input (hash "pbga" (program "pbga" 66 '())
                      "xhth" (program "xhth" 57 '())
                      "ebii" (program "ebii" 61 '())
                      "havc" (program "havc" 66 '())
                      "ktlj" (program "ktlj" 57 '())
                      "fwft" (program "fwft" 72 (list "ktlj" "cntj" "xhth"))
                      "qoyq" (program "qoyq" 66 '())
                      "padx" (program "padx" 45 (list "pbga" "havc" "qoyq"))
                      "tknk" (program "tknk" 41 (list "ugml" "padx" "fwft"))
                      "jptl" (program "jptl" 61 '())
                      "ugml" (program "ugml" 68 (list "gyxo" "ebii" "jptl"))
                      "gyxo" (program "gyxo" 61 '())
                      "cntj" (program "cntj" 57 '())))
  (define tower (index->tower input))
  (check-equal? (program-name tower) "tknk")
  (check-equal? (rebalance tower) 60))

(define (parse line)
  (match line
    [(pregexp #px"^(\\w+) \\((\\d+)\\)$" (list _ name weight))
     (program name (string->number weight) '())]
    [(pregexp #px"^(\\w+) \\((\\d+)\\) -> (.*)$" (list _ name weight children))
     (program name (string->number weight) (regexp-split #px",\\s*" children))]))

(define (bottom-program input)
  (define index (for/hash ([line (in-lines input)])
                  (define prog (parse line))
                  (values (program-name prog) prog)))
  (define tower (index->tower index))
  (printf "Puzzle A: ~a\n" (program-name tower))
  (printf "Puzzle B: ~a\n" (rebalance tower)))

(call-with-input-file "day-07-input.txt" bottom-program #:mode 'text)