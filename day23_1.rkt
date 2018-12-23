#lang racket

(define (manhatten p q)
  (+ (abs (- (first p) (first q)))
     (abs (- (second p) (second q)))
     (abs (- (third p) (third q)))))

(define (solve-puzzle inp)
  (define p (argmax fourth inp))
  (define r (fourth p))
  (length (filter (lambda (q) (>= r (manhatten p q))) inp)))
  
(define (parse-input inp)
  (define (parse-line line)
    (map string->number (drop (regexp-match #px"pos=<((-?\\d+),(-?\\d+),(-?\\d+))>, r=(\\d+)" line) 2)))
  (map parse-line inp))

(define test-input
  (list "pos=<0,0,0>, r=4"
        "pos=<1,0,0>, r=1"
        "pos=<4,0,0>, r=3"
        "pos=<0,2,0>, r=1"
        "pos=<0,5,0>, r=3"
        "pos=<0,0,3>, r=1"
        "pos=<1,1,1>, r=1"
        "pos=<1,1,2>, r=1"
        "pos=<1,3,1>, r=1"))