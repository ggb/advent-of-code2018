#lang racket

(require data/gvector)

(define (find-recipes positions x y len i target)
  (if (and (> len 10)
              (or (equal? target (map (curry gvector-ref positions) (range (- len (length target) 1) (sub1 len))))
                  (equal? target (map (curry gvector-ref positions) (range (- len (length target)) len)))))
      (- len (length target))
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
        (find-recipes positions new-x new-y new-len (add1 i) target))))

(define (solve-puzzle recipes)
  (find-recipes (gvector 3 7) 0 1 2 0 (map (compose string->number string) (string->list recipes))))

;(time (solve-puzzle "190221"))