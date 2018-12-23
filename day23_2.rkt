#lang rosette/safe

(define test-input
  (list '(10 12 12 2)
        '(12 14 12 2)
        '(16 12 12 4)
        '(14 14 14 6)
        '(50 50 50 200)
        '(10 10 10 5)))

(define (manhatten x y z a b c)
  (+ (abs (- x a))
     (abs (- y b))
     (abs (- z c))))

(define-symbolic x y z integer?)

(define (in-range x y z)
  (apply + (map (lambda (l)
                  (if (<= (manhatten x y z (first l) (second l) (third l)) (fourth l))
                      1
                      0))
                test-input)))

(define sol
    (optimize #:maximize (in-range x y z)
              #:minimize (manhatten 0 0 0 x y z)
              #:guarantee (assert true)))

(evaluate x sol)
(evaluate y sol)
(evaluate z sol)