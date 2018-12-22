#lang racket

(require threading)

(define (calc-erosion-level x y target-x target-y depth coords)
  (define geologic-index (cond
                           [(and (= x 0) (= y 0)) 0]
                           [(and (= x target-x) (= y target-y)) 0]
                           [(= y 0) (* x 16807)]
                           [(= x 0) (* y 48271)]
                           [else (* (hash-ref coords (list (- x 1) y)) (hash-ref coords (list x (- y 1))))]))
  (modulo (+ geologic-index depth) 20183))

(define (calc-risk-level x y depth)
  (define target-x (+ x 1))
  (define target-y (+ y 1))
  (~>> (cartesian-product (range target-x) (range target-y))
       (foldl (lambda (p acc)
                (hash-set acc p (calc-erosion-level (first p) (second p) target-x target-y depth acc)))
              (hash))))

(define (solve-puzzle x y depth)
  (~>> (calc-risk-level x y depth)
       (hash-values)
       (map (lambda (e) (modulo e 3)))
       (apply +)))

;(solve-puzzle 9 796 6969)