#lang racket

(require threading)

; "12. Recursion is the root of computation since it trades description for time."
; Alan Perlis

(provide solve-puzzle solve-puzzle-fast test-input ul-eq?)

(define (fst str)
  (if (< 0 (string-length str))
      (substring str 0 1)
      ""))

(define (rst str)
  (substring str 1))

(define (fst-rst str)
  (fst (rst str)))

(define (rst-rst str)
  (rst (rst str)))

(define (ul-eq? a b)
  (and (equal? (string-downcase a) (string-downcase b))
       (not (equal? a b))))

(define (reduce-polarity str new-str changed?)
  (if (equal? "" str)
      (cons changed? new-str)
      (if (ul-eq? (fst str) (fst-rst str))
          (reduce-polarity (rst-rst str) new-str #t)
          (reduce-polarity (rst str) (string-append new-str (fst str)) changed?))))

(define (solve-puzzle input)
  (let ([res (reduce-polarity input "" #f)])
    (if (car res)
        (solve-puzzle (cdr res))
        (string-length (cdr res)))))

; ... but there is a mutch faster solution:

(define (save-first lst)
  (if (empty? lst)
      ""
      (first lst)))

(define (solve-puzzle-fast input)
  (~>> input
       (string->list)
       (map string)
       (foldr
        (lambda (ele acc)
            (if (ul-eq? ele (save-first acc))
                (rest acc)
                (cons ele acc)))
        (list))
       (length)))

; Tests

(define test-input "dabAcCaCBAcCcaDA")
(= 10 (solve-puzzle test-input))
(= 10 (solve-puzzle-fast test-input))

; (solve-puzzle (first (file->lines "day5_input.txt")))
; (solve-puzzle-fast (first (file->lines "day5_input.txt")))