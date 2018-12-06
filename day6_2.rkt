#lang racket

(require threading)
(require "day6_1.rkt")

(define (solve-puzzle input)
  (~>> (points 50 50 350 350)
       (map (lambda (p)
              (~>> (map (curry distance p) input))))
       (map (lambda (x) (foldl (lambda (ele acc) (+ acc (car ele))) 0 x)))
       (filter (curry > 10000))
       (length)))