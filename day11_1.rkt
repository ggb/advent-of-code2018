#lang racket

(provide solve-puzzle*)

(define (power-level serial-number point)
  (let ([rack-id (+ (first point) 10)])
    (- (modulo (quotient (* rack-id (+ (* rack-id (second point)) serial-number)) 100) 10) 5)))

(define (power-square serial-number square-size point)
  (let ([x (first point)]
        [y (second point)])
    (cons point
          (apply + (map (curry power-level serial-number)
                        (cartesian-product (range x (+ x square-size)) (range y (+ y square-size))))))))

(define (solve-puzzle* serial-number square-size)
  (argmax cdr
          (map (curry power-square serial-number square-size) (cartesian-product (range 1 (- 301 square-size))
                                                                                 (range 1 (- 301 square-size))))))

(define (solve-puzzle)
  (solve-puzzle* 4842 3))

(=  4 (power-level 8 (list 3 5)))
(= -5 (power-level 57 (list 122 79)))
(=  0 (power-level 39 (list 217 196)))
(=  4 (power-level 71 (list 101 153)))