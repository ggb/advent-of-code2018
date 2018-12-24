#lang racket

(require "day11_1.rkt")

(define (solution-for-square n)
  (let ([solution (solve-puzzle* n)])
    (list (cdr solution) (car solution) n)))

(define (solve-puzzle)
  (define max-square (argmax first (map solution-for-square (range 1 300))))
  (define origin (second max-square))
  (define size (third max-square))
  (printf "~a,~a,~a" (first origin) (second origin) size))

;(solve-puzzle)