#lang racket

(require "day16_1.rkt")
(require threading)

(define (create-mappings op-code inp-a inp-b inp-c before after)
  (let* ([before-hash (list->hash before)]
         [after-hash (list->hash after)]
         [unique-result? (~>> (range 16)
                              (map (curry run-instruction inp-a inp-b inp-c before-hash))
                              (map cons (range 16))
                              (filter (lambda (x) (equal? after-hash (cdr x))))
                              (map car)
                              (map (lambda (x) (cons x op-code))))])
    unique-result?))

(define (find-mapping l)
  (~>> l
       (map cdr)
       (group-by identity)
       (map (lambda (x) (cons (first x) (length x))))))

(define (solve-puzzle parsed-input)
  (let ([results  (~>> parsed-input
                       (map (lambda (m) (let ([s (second m)])
                                          (create-mappings (first s) (second s) (third s) (fourth s) (first m) (third m)))))
                       (filter (negate empty?))
                       (foldl append '()))])
    (map (lambda (x) (cons x (find-mapping (filter (lambda (y) (= x (car y))) results)))) (range 16))))

; (solve-puzzle (parse-input "day16_input1.txt"))
; Use output to gain the mapping by hand.