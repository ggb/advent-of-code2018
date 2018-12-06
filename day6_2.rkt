#lang racket

(require threading)
(require "day6_1.rkt")

(define (solve-puzzle n input)
  (~>> (extreme-coords input)
       (apply points)
       (map (lambda (p)
              (~>> (map (curry distance p) input)
                   (foldl (lambda (ele acc) (+ acc (car ele))) 0))))
       (filter (curry > n))
       (length)))

(= 16 (solve-puzzle 32 test-input))

; (solve-puzzle 10000 (parse-input "day6_input.txt"))