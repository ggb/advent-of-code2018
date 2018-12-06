#lang racket

(require threading)

(provide distance points parse-input test-input extreme-coords)

(define (distance p q)
  (cons (+ (abs (- (car p) (car q)))
           (abs (- (cdr p) (cdr q))))
        q))

(define (points offset-x offset-y width height)
  (foldl append (list)
         (for/list ([x (in-range offset-x (+ offset-x width))])
           (for/list ([y (in-range offset-y (+ offset-y height))])
             (cons x y)))))

(define (same-min? l)
  (let* ([min (argmin car l)]
         [more? (< 1 (length (filter (lambda (x) (= (car x) (car min))) l)))])
    (if more?
        (cons 0 '())
        min)))

(define (extreme-coords input)
  (list (car (argmin car input))
        (cdr (argmin cdr input))
        (car (argmax car input))
        (cdr (argmax cdr input))))

(define (solve-puzzle input)
  (~>> (extreme-coords input)
       (apply points)
       (map (lambda (p)
              (~>> (map (curry distance p) input)
                   (same-min?)
                   (cdr))))
       (group-by identity)
       (map (lambda (x) (cons (length x) (first x))))))

(define (parse-input file)
  (map (lambda (l)
         (let ([p (string-split l ", ")])
           (cons (string->number (first p))
                 (string->number (second p)))))
       (file->lines file)))

(define test-input
  (list (cons 1 1) (cons 1 6) (cons 8 3) (cons 3 4) (cons 5 5) (cons 8 9)))

; (sort (solve-puzzle (parse-input "day6_input.txt")) #:key car <)

; I was to lazy to implement the removal of inifinite areas; but the result is
; easy to spot: It's the highest value were x and y < 300.