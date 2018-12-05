#lang racket

(require "day5_1.rkt")
(require threading)

(define (get-chars input)
  (~>> input
       (string->list)
       (map (compose string-downcase string))
       (list->set)
       (set->list)))

(define (filter-helper search-char)
  (lambda (ele)
    (not (equal? search-char (string-downcase ele)))))

(define (solve input)
  (~>> input
       (get-chars)
       (map (lambda (search-char)
              (cons search-char
                    (solve-puzzle-fast
                     (string-join (~>> input
                                       (string->list)
                                       (map string)
                                       (filter (filter-helper search-char))) "")))))
       (argmin cdr)))

; (solve (first (file->lines "day5_input.txt")))