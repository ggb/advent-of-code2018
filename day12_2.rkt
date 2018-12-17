#lang racket

(require "day12_1.rkt")

; Look for a pattern
(define inputs
  (list 20 100 200 300 500 1000 2000 2001 2002 2003 3000))

(for ([i inputs])
  (println
   (time
    (solve-puzzle i #f (file->lines "day12_input.txt")))))

; Pattern: Iteration Result for 2002 is IR for 2001 + 75 is IR for 2000 + 75 ...
(+ 151113 (* 75 (- 50000000000 2000)))
