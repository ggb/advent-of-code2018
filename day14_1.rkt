#lang racket

(require threading)

(define (create-recipes positions x y len i max-iter)
  (if (= i max-iter)
      positions
      (let* ([m (hash-ref positions x)]
             [n (hash-ref positions y)]
             [new-m (quotient (+ m n) 10)]
             [new-n (modulo (+ m n) 10)]
             [new-positions (if (= 0 new-m)
                                (hash-set positions len new-n)
                                (hash-set (hash-set positions len new-m) (+ len 1) new-n))]
             [new-len (if (= 0 new-m) (+ len 1) (+ len 2))]
             [new-x (modulo (+ x (add1 m)) new-len)]
             [new-y (modulo (+ y (add1 n)) new-len)])
        (create-recipes new-positions new-x new-y new-len (add1 i) max-iter))))

(define (solve-puzzle recipe-count n)
  (let ([recipes (create-recipes #hash((0 . 3) (1 . 7)) 0 1 2 0 (+ recipe-count n))])
    (string-join (~>> (range recipe-count (+ recipe-count n))
                      (map (curry hash-ref recipes))
                      (map number->string)) "")))

; (time (solve-puzzle 190221 10))