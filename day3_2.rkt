#lang racket

(require "day3_1.rkt")
(require threading)

(define (parse-input input-str)
  (~>> (string-split input-str #px"\\@|,|:|x")
       (rest)
       (map (compose string->number string-trim))))

(define (revert-to-dim pair-list)
  (let* ([pairs (set->list (first pair-list))]
         [fst (map car pairs)]
         [scd (map cdr pairs)]
         [min-fst (apply min fst)]
         [min-scd (apply min scd)])
    (format "~a,~a: ~ax~a"
            min-fst
            min-scd
            (add1 (- (apply max fst) min-fst))
            (add1 (- (apply max scd) min-scd)))))
 
(define (solve-puzzle input)
  (let* ([overlap (~>> (solve-puzzle* input)
                       (map first)
                       (list->set))]
         [target-dims (~>> input
                          (map parse-input)
                          (map (λ (e) (list->set (apply elf-squares e))))
                          (filter (lambda (e) (set-empty? (set-intersect e overlap))))
                          (revert-to-dim))])
    (first (filter (λ (e) (string-contains? e target-dims)) input))))

(define test-input 
  (list "#1 @ 1,3: 4x4" "#1 @ 3,1: 4x4" "#3 @ 5,5: 2x2"))

(equal? "#3 @ 5,5: 2x2" (solve-puzzle test-input))
