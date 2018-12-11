#lang racket

(require "day11_1.rkt")

(define (solution-for-square n)
  (let ([solution (time (solve-puzzle* 4842 n))])
    (list (cdr solution) (car solution) n)))

(define chunks
  (list (range 1 76) (range 76 151) (range 151 226) (range 226 301)))

(define (solve-puzzle)
  (let ([f1 (lambda () (argmax first (map solution-for-square (first chunks))))]
        [f2 (future (lambda () (argmax first (map solution-for-square (second chunks)))))]
        [f3 (future (lambda () (argmax first (map solution-for-square (third chunks)))))]
        [f4 (future (lambda () (argmax first (map solution-for-square (fourth chunks)))))])
    (argmax first (list (f1) (touch f2) (touch f3) (touch f4)))))

;(solve-puzzle)