#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(provide parse-input draw-forest update-forest)

(struct world (open-ground trees lumberyard width height iteration))

(define acre-size 10)

(define open-ground
  (square acre-size 'solid 'white))

(define lumber-yard
  (square acre-size 'solid 'brown))

(define tree
  (circle (/ acre-size 2) 'solid 'green))

(define (draw-row trees lumberyard width line-number)
  (apply beside (map (lambda (i)
                       (cond
                         [(set-member? trees (list i line-number)) tree]
                         [(set-member? lumberyard (list i line-number)) lumber-yard]
                         [else open-ground]))
                     (range width))))

(define (draw-forest w)
  (apply above (map (curry draw-row (world-trees w) (world-lumberyard w) (world-width w)) (range (world-height w)))))

(define (current-field-content old-w p)
  (cond
    [(set-member? (world-trees old-w) p) "|"]
    [(set-member? (world-lumberyard old-w) p) "#"]
    [else "."]))

(define (rule-helper target min old-w p)
  (let ([x (first p)]
        [y (second p)])
    (<= min (set-count
             (set-intersect (target old-w)
                            (apply set (cartesian-product (range (- x 1) (+ x 2)) (range (- y 1) (+ y 2)))))))))
; Rule 1
(define (open-space->tree? old-w p)
  (rule-helper world-trees 3 old-w p))

; Rule 2
(define (trees->lumberyard? old-w p)
  (rule-helper world-lumberyard 3 old-w p))

; Rule 3
(define (stay-lumberyard? old-w p)
  (and (rule-helper world-trees 1 old-w p)
       (rule-helper world-lumberyard 2 old-w p)))

(define (update-acre old-w p new-w)
  (let ([current (current-field-content old-w p)])
    (cond
      [(and (open-space->tree? old-w p)
            (equal? current ".")) (struct-copy world new-w [trees (set-add (world-trees new-w) p)])]
      [(equal? current ".") (struct-copy world new-w [open-ground (set-add (world-open-ground new-w) p)])]
      [(and (trees->lumberyard? old-w p)
            (equal? current "|")) (struct-copy world new-w [lumberyard (set-add (world-lumberyard new-w) p)])]
      [(equal? current "|") (struct-copy world new-w [trees (set-add (world-trees new-w) p)])]
      [(and (stay-lumberyard? old-w p)
            (equal? current "#")) (struct-copy world new-w [lumberyard (set-add (world-lumberyard new-w) p)])]
      [(equal? current "#") (struct-copy world new-w [open-ground (set-add (world-open-ground new-w) p)])])))

(define (eval-resource-value state)
  (* (set-count (world-trees state)) (set-count (world-lumberyard state))))

(define (update-forest state)
  (let ([new-state (foldl (curry update-acre state)
                          (world (set) (set) (set) (world-width state) (world-height state) (add1 (world-iteration state)))
                          (cartesian-product (range (world-width state)) (range (world-height state))))])
    (begin
      (printf "Iteration: ~a, Resource value: ~a~n" (world-iteration new-state) (eval-resource-value new-state))
      new-state)))

(define (parse-line linei state)
  (let* ([y-index (cdr linei)]
         [split-line (drop-right (drop (string-split (car linei) "") 1) 1)]
         [new-width (max (length split-line) (world-width state))])
    (foldl (lambda (ei s)
             (cond
               [(equal? (car ei) ".") (struct-copy world s [open-ground (set-add (world-open-ground s) (list (cdr ei) y-index))])]
               [(equal? (car ei) "#") (struct-copy world s [lumberyard (set-add (world-lumberyard s) (list (cdr ei) y-index))])]
               [(equal? (car ei) "|") (struct-copy world s [trees (set-add (world-trees s) (list (cdr ei) y-index))])]))
           (struct-copy world state [width new-width])
           (map cons split-line (range (length split-line))))))

(define (parse-input lines)
  (foldl (curry parse-line) (world (set) (set) (set) 0 (length lines) 0) (map cons lines (range (length lines)))))

(define (solve-puzzle init-state)
  (big-bang
      init-state
    (on-tick update-forest 2 10)
    (to-draw draw-forest)))

(define test-input
  (list ".#.#...|#."
        ".....#|##|"
        ".|..|...#."
        "..|#.....#"
        "#.#|||#|#|"
        "...#.||..."
        ".|....|..."
        "||...#|.#|"
        "|.||||..|."
        "...#.|..|."))

; (solve-puzzle (parse-input (file->lines "day18_input.txt")))