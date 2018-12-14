#lang racket

(require threading)
(require data/gvector)

(define (create-recipes positions x y len i max-iter)
  (if (= i max-iter)
      positions
      (let* ([m (gvector-ref positions x)]
             [n (gvector-ref positions y)]
             [new-m (quotient (+ m n) 10)]
             [new-n (modulo (+ m n) 10)]
             [_ (if (= 0 new-m)
                    (gvector-insert! positions len new-n)
                    (begin
                      (gvector-insert! positions len new-m)
                      (gvector-insert! positions (+ len 1) new-n)))]
             [new-len (if (= 0 new-m) (+ len 1) (+ len 2))]
             [new-x (modulo (+ x (add1 m)) new-len)]
             [new-y (modulo (+ y (add1 n)) new-len)])
        (create-recipes positions new-x new-y new-len (add1 i) max-iter))))

(define (solve-puzzle recipe-count n)
  (let ([recipes (create-recipes (gvector 3 7) 0 1 2 0 (+ recipe-count n))])
    (string-join (~>> (range recipe-count (+ recipe-count n))
                      (map (curry gvector-ref recipes))
                      (map number->string)) "")))

(time (solve-puzzle 20268576 10))