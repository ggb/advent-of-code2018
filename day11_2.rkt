#lang racket

(require pmap)
(require "day11_1.rkt")

(define (solution-for-square n)
  (let ([solution (time (solve-puzzle* 4842 n))])
    (list (cdr solution) (car solution) n)))

(define chunks
  (list (range 1 76) (range 76 151) (range 151 226) (range 226 301)))

(define (solve-puzzle)
  (argmax (pmapf (lambda (chunk) (argmax first (map solution-for-square chunk))) chunks)))

(solve-puzzle)