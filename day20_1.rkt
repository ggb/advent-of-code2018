#lang racket

(require graph)

(define (solve-puzzle input)
  (define parsed-input (drop (drop-right (string-split input "") 2) 2))
  parsed-input)

(define t1 "^WNE$")
(define t2 "^ENWWW(NEEE|SSE(EE|N))$")
(define t3 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")

(= 3 (solve-puzzle t1))
(= 10 (solve-puzzle t2))
(= 18 (solve-puzzle t3))