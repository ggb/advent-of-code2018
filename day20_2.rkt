#lang racket

(require graph)
(require "day20_1.rkt")

(define (solve-puzzle input)
  (define g (regex->graph (prepare-input input)))
  (let-values ([(distances _) (bfs g '(0 0))])
    (length (filter (curry <= 1000) (hash-values distances)))))

; (solve-puzzle (first (file->lines "day20_input.txt")))