#lang racket

(require threading)
(require "day13_1.rkt")

(define (update-carts track carts)
  (define first-cart (first carts))
  (define updated-cart (move-cart track first-cart))
  (define updated-carts (append (rest carts) (list updated-cart)))
  (define equal-pos-count (length (filter (lambda (ct) (equal? (cart-pos ct) (cart-pos updated-cart))) updated-carts)))
  (if (= 2 equal-pos-count)
      (filter (lambda (ct) (not (equal? (cart-pos ct) (cart-pos updated-cart)))) updated-carts)
      updated-carts))

(define (solve-puzzle* iterations carts track)
  (if (or (> iterations 1000000)
          (<= (length carts) 1))
      (begin
        (println iterations)
      (cart-pos (first carts)))
      (solve-puzzle* (add1 iterations) (update-carts track carts) track)))

(define (solve-puzzle inp)
  (define parsed (parse-input inp))
  (solve-puzzle* 0 (car parsed) (cdr parsed)))

(define test-input
  (list "/>-<\\  "
        "|   |  "
        "| /<+-\\"
        "| | | v"
        "\\>+</ |"
        "  |   ^"
        "  \\<->/"))