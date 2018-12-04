#lang racket

(require threading)

(require "day4_1.rkt")

(define (argmax-helper l)
  (if (empty? l)
      (cons 0 0)
      (argmax cdr l)))

(define (solve-puzzle input)
  (let* ([sleep-hash (~>> (sort input string<?)
                          (create-sleep-list (hash) #f #f))]
         [minutes-per-guard (hash-map sleep-hash
                                      (lambda (k v) (list k (~>> v
                                                                 (map (λ (iv) (range (car iv) (cdr iv))))
                                                                 (flatten)
                                                                 (group-by identity)
                                                                 (map (λ (v) (cons (first v) (length v))))
                                                                 (argmax-helper)))))])
    (argmax cdadr minutes-per-guard)))

; (solve-puzzle (file->lines "day4_input.txt"))