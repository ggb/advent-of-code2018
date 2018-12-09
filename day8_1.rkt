#lang racket

(require threading)

(define (solve-children children rest-list)
  (foldl (lambda (x acc)
           (let ([res (solve x (cdr acc))])
             (cons (+ (car acc) (car res)) (cdr res))))
         (cons 0 rest-list)
         (reverse (range children))))

(define (solve n l)
  (begin
    (println l)
  (if (empty? l)
      (cons 0 '())
      (let ([children (first l)]
            [metadata (second l)])
        (if (= children 0)
            (cons (+ (apply + (take (drop l 2) metadata)))
                  (drop l (+ metadata 2)))
            (if (= n 0)
                (cons (+ (apply + (take-right l metadata))
                         (car (solve-children children (drop-right (drop l 2) metadata))))
                      '())
                (solve-children children (drop l 2))))))))

(define (parse-input inp)
  (~>> inp
       (string-split)
       (map string->number)))

(define (solve-puzzle inp)
  (first (solve 0 (parse-input inp))))

(define test-input  "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
(define test-input2 "2 3 1 1 0 1 99 2 0 3 10 11 12 1 1 2")

(= 138 (solve-puzzle test-input))
(= 138 (solve-puzzle test-input2))
