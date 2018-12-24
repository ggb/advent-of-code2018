#lang racket

(provide power-level solve-puzzle*)

(define serial-number 4842)

(define (square-points x1 x2 y1 y2)
  (cartesian-product (range x1 x2)
                     (range y1 y2)))

(define (power-level serial-number point)
  (let ([rack-id (+ (first point) 10)])
    (- (modulo (quotient (* rack-id (+ (* rack-id (second point)) serial-number)) 100) 10) 5)))

(define (create-power-sum-table power-levels)
  (foldl (lambda (p acc)
           (define x (first p))
           (define y (second p))
           (hash-set acc p (+ (hash-ref power-levels p)
                              (hash-ref acc (list x (sub1 y)) 0)
                              (- (hash-ref acc (list (sub1 x) y) 0)
                                 (hash-ref acc (list (sub1 x) (sub1 y)) 0)))))
         (hash)
         (square-points 1 301 1 301)))

(define power-levels (make-immutable-hash
                        (map (lambda (p)
                               (cons p (power-level serial-number p)))
                             (square-points 1 301 1 301))))

(define power-sum-table (create-power-sum-table power-levels))

(define (power-square square-size point)
  (let ([x (first point)]
        [y (second point)])
    (cons (list (add1 x) (add1 y))
          (- (+ (hash-ref power-sum-table (list x y))
                (hash-ref power-sum-table (list (+ x square-size) (+ y square-size))))
             (hash-ref power-sum-table (list x (+ y square-size)))
             (hash-ref power-sum-table (list (+ x square-size) y))))))

(define (solve-puzzle* square-size)
  (argmax cdr
          (map (curry power-square square-size) (square-points 1 (- 301 square-size)
                                                               1 (- 301 square-size)))))

(define (solve-puzzle)
  (define origin (car (solve-puzzle* 3)))
  (printf "~a,~a" (first origin) (second origin)))

(=  4 (power-level 8 (list 3 5)))
(= -5 (power-level 57 (list 122 79)))
(=  0 (power-level 39 (list 217 196)))
(=  4 (power-level 71 (list 101 153)))

; (solve-puzzle)