#lang racket

(require threading)

(define (find-equals full-list word)
  (~>> full-list
       (map (λ (ele)
                 (foldr (λ (ele acc)
                          (if (equal? (car ele) (cdr ele))
                              (cons (car ele) acc)
                              acc))
                        (list)
                        (map cons word ele))))
       (filter (λ (e) (= (sub1 (length word))
                         (length e))))))

(define (solve-puzzle input)
  (let ([mapped (map string->list input)])
    (~>> mapped
         (map (curry find-equals mapped))
         (filter (compose not empty?))
         (first)
         (flatten)
         (list->string))))

(define test-input
  (list "abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"))

(equal? "fgij" (solve-puzzle test-input))

;(solve-puzzle (file->lines "day2_1-input.txt"))