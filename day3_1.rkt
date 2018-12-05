#lang racket

(require threading)

(provide elf-squares parse-input solve-puzzle*)

(define (elf-squares offset-x offset-y width height)
  (foldl append (list)
         (for/list ([x (in-range offset-x (+ offset-x width))])
           (for/list ([y (in-range offset-y (+ offset-y height))])
             (cons x y)))))

(define (parse-input input-str)
  (~>> (string-split input-str #px"\\@|,|:|x")
       (rest)
       (map (compose string->number string-trim))))

(define (solve-puzzle* input)
  (~>> input
       (map parse-input)
       (map (λ (e) (apply elf-squares e)))
       (foldl append '())
       (group-by identity)
       (filter (λ (v) (> (length v) 1)))))

(define (solve-puzzle input)
  (length (solve-puzzle* input)))

(define test-input 
  (list "#1 @ 1,3: 4x4" "#1 @ 3,1: 4x4" "#3 @ 5,5: 2x2"))

(= 4 (solve-puzzle test-input))

; (solve-puzzle (file->lines "day3_input.txt"))