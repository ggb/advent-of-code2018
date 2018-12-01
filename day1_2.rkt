#lang racket

(require racket/set)

(define (reduce-and-find* input-list input-list-copy last-val seen?)
  (if (empty? input-list)
      (reduce-and-find* input-list-copy input-list-copy last-val seen?)
      (let ([current-number (+ last-val (first input-list))])
        (begin
          ;(printf "- Current frequency ~a, change of ~a; resulting frequency  ~a.\n" last-val (first input-list) current-number)
          (if (set-member? seen? current-number)
              current-number
              (reduce-and-find* (rest input-list) input-list-copy current-number (set-add seen? current-number)))))))

(define (reduce-and-find input-list)
  (reduce-and-find* input-list input-list 0 (set 0)))

(=  2 (reduce-and-find '(1 -2 3 1 1 -2)))
(=  0 (reduce-and-find '(1 -1)))
(= 10 (reduce-and-find '(3 3 4 -2 -4)))
(=  5 (reduce-and-find '(-6 3 8 5 -6)))
(= 14 (reduce-and-find '(7 7 -2 -7 -4)))

(reduce-and-find (map string->number (file->lines "day1_1-input.txt")))