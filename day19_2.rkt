#lang racket

(require threading)

(define (solve-puzzle target-number)
  (~>> target-number
       (add1)
       (range 1)
       (filter (lambda (i) (= 0 (modulo target-number i))))
       (apply +)))

; observe execution of day19_1.rkt with reg0 = 1 to get the
; input number (appears after the first few instructions)
(solve-puzzle 10551305)