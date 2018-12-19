#lang racket

(require 2htdp/universe)
(require "day18_1.rkt")

(define (solve-puzzle init-state)
  (big-bang
      init-state
    (on-tick update-forest 0.3) ; higher frame-rate, no stop condition
    (to-draw draw-forest)
    (record? #f)))

; (solve-puzzle (parse-input (file->lines "day18_input.txt")))

; Look at the output: There is a pattern that repeats every 35 iterations.
; To get the value for the 1000000000th iteration, take a starting point (in my case 521)
; and substracts its iteration number from 1000000000. Take the result modulo 35 (the
; "loop" size) and add the starting point again. The resulting number is an iteration
; with the same value as the 1000000000th one.

; (+ (modulo (- 1000000000 521) 35) 521)