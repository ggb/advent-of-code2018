#lang racket

(require graph)

(provide prepare-input regex->graph)

(define (direction->vector dir)
  (cond
    [(equal? dir "N") (list  0 -1)]
    [(equal? dir "E") (list  1  0)]
    [(equal? dir "S") (list  0  1)]
    [(equal? dir "W") (list -1  0)]))

(define (dir-add pos dir)
  (map + pos (direction->vector dir)))

(define (create-door re pos graph coords stack)
  (define new-coords (dir-add coords (vector-ref re pos)))
  (begin
    (add-edge! graph coords new-coords)
    (parse-char re (add1 pos) graph new-coords stack)))

(define (parse-char re pos graph coords stack)
  (if (<= (vector-length re) pos)
      graph
      (let ([c (vector-ref re pos)])
        (cond
          [(equal? c "(") (parse-char re (add1 pos) graph coords (cons coords stack))]
          [(equal? c "|") (parse-char re (add1 pos) graph (first stack) stack)]
          [(equal? c ")") (parse-char re (add1 pos) graph (first stack) (rest stack))]
          [else (create-door re pos graph coords stack)]))))

(define (regex->graph re)
  (define g (unweighted-graph/undirected '()))
  (parse-char (list->vector re) 0 g (list 0 0) '())
  g)

(define (prepare-input inp)
  (drop (drop-right (string-split inp "") 2) 2))

(define (solve-puzzle input)
  (define g (regex->graph (prepare-input input)))
  (let-values ([(distances _) (bfs g '(0 0))])
    (apply max (hash-values distances))))

(define t1 "^WNE$")
(define t2 "^ENWWW(NEEE|SSE(EE|N))$")
(define t3 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")

(= 3 (solve-puzzle t1))
(= 10 (solve-puzzle t2))
(= 18 (solve-puzzle t3))

; (solve-puzzle (first (file->lines "day20_input.txt")))