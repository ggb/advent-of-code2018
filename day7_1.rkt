#lang racket

(require threading)

(define (not-available? pairs target)
  (~>> pairs
       (filter (λ (p) (equal? target (cdr p))))
       (map car)
       (length)
       (< 1)
       (not)))

(define (find-next pairs start)
  (sort 
   (filter (λ (p) (equal? start (car p))) pairs)
   #:key cdr
   string<?))

(define (find-start pairs)
  (sort
   (set-subtract
    (map car pairs)
    (map cdr pairs))
   string<?))

(define (solve pairs source visited open)
  (if (empty? pairs)
      visited
      (let* ([nexts (find-next pairs source)]
             [next-pairs (set-subtract pairs nexts)]
             [targets (filter (curry not-available? pairs) (map cdr nexts))])
        (if (empty? targets)
            (solve next-pairs (first open) (cons (first open) visited) (rest open))
            (solve next-pairs (first targets) (cons (first targets) visited) (append (rest targets) open))))))

(define (solve-puzzle input)
  (let* ([pairs (parse-input input)]
         [open (find-start pairs)]
         [start (first open)])
    (string-join
     (reverse (solve pairs start (list start) (rest open)))
     "")))

(define (parse-input input)
  (map
   (λ (i)
     (let ([m (regexp-match #px"Step (\\w) must be finished before step (\\w) can begin." i)])
       (cons (second m) (third m))))
   input))

(define test-input
  (list "Step C must be finished before step A can begin."
        "Step C must be finished before step F can begin."
        "Step A must be finished before step B can begin."
        "Step A must be finished before step D can begin."
        "Step B must be finished before step E can begin."
        "Step D must be finished before step E can begin."
        "Step F must be finished before step E can begin."))

(equal? "CABDFE" (solve-puzzle test-input))

; (solve-puzzle (file->lines "day7_input.txt"))