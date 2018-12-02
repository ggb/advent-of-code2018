#lang racket

(define (count-reduce char-list)
  (foldr (lambda (ele acc)
           (hash-update acc ele (lambda (v) (+ v 1)) 0))
         (hash)
         char-list))

(define (has-two-or-three char-hash)
  (cons (if (member 2 (hash-values char-hash)) 1 0)
        (if (member 3 (hash-values char-hash)) 1 0)))

(define (reduce-pairs pair-list)
  (foldr (lambda (ele acc)
           (cons (+ (car ele) (car acc))
                 (+ (cdr ele) (cdr acc))))
          (cons 0 0)
          pair-list))

(define (solve-puzzle str-list)
  (let ([result-pair (reduce-pairs (map has-two-or-three
                                        (map count-reduce
                                             (map string->list str-list))))])
    (* (car result-pair) (cdr result-pair))))

(define test-inputs
  (list "abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"))

(= 12 (solve-puzzle test-inputs))

;(solve-puzzle (file->lines "day2_1-input.txt"))