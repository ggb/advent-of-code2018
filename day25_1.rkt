#lang racket

(require graph)
(require threading)

(define (manhatten p q)
  (define (diff x y) (abs (- x y)))
  (apply + (map diff p q)))

(define (neighbours all-points p)
  (~>> all-points
       (filter (λ (q) (>= 3 (manhatten p q))))
       (map (curry list p))))

(define (parse-input inp)
  (define (parse-line l)
    (map string->number (string-split l ",")))
  (map parse-line inp))

(define (solve-puzzle inp)
  (~>> inp
       (map (curry neighbours inp))
       (append*)
       (unweighted-graph/undirected)
       (cc)
       (length)))

(define test-input
  (list "-1,2,2,0"
        "0,0,2,-2"
        "0,0,0,-2"
        "-1,2,0,0"
        "-2,-2,-2,2"
        "3,0,2,-1"
        "-1,3,2,2"
        "-1,0,-1,0"
        "0,2,1,-2"
        "3,0,0,0"))

; (solve-puzzle (parse-input (file->lines "day25_input.txt")))