#lang racket

(require "day21_1.rkt")

(define (solve-puzzle* inp reg-a store)
  (let* ([reg-b (bitwise-ior reg-a 65536)]  ; line 06
         [new-reg-a (elf-loop inp reg-b)])
    (if (set-member? store new-reg-a)
        reg-a
        (solve-puzzle* inp new-reg-a (set-add store new-reg-a)))))

(define (solve-puzzle inp)
  (solve-puzzle* inp 0 (set)))

(solve-puzzle 10905776)