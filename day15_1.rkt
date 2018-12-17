#lang racket

(require threading)
(require 2htdp/image)
(require 2htdp/universe)

(define empty-space
  (square 30 'solid 'white))

(define rock
  (square 30 'solid 'brown))

(define goblin
  (square 30 'solid 'green))

(define elf
  (square 30 'solid 'red))

(define (update-fight state)
  state)

(define (draw-row rocks goblins elfs width line-number)
  (apply beside (map (lambda (i)
                       (cond
                         [(set-member? rocks (cons i line-number)) rock]
                         [(set-member? elfs (cons i line-number)) elf]
                         [(set-member? goblins (cons i line-number)) goblin]
                         [else empty-space]))
                     (range width))))

(define (draw-fight state)
  (let ([rocks (first state)]
        [goblins (second state)]
        [elfs (third state)]
        [width (fourth state)]
        [height (fifth state)])
    (apply above (map (curry draw-row rocks goblins elfs width) (range height)))))

(define (solve-puzzle init-state)
  (big-bang
      init-state
    (on-tick update-fight)
    (to-draw draw-fight)))

(define (parse-line rocks goblins elfs line line-number)
  (let ([elements (drop-right (drop (string-split line "") 1) 1)])
    (map (lambda (e i)
           (cond
             [(equal? e "#") (set-add! rocks (cons i line-number))]
             [(equal? e "G") (set-add! goblins (cons i line-number))]
             [(equal? e "E") (set-add! elfs (cons i line-number))]))
         elements (range (length elements)))))

(define (parse-input file-name)
  (let ([lines (file->lines file-name)]
        [rocks (mutable-set)]
        [goblins (mutable-set)]
        [elfs (mutable-set)])
    (map (curry parse-line rocks goblins elfs) lines (range (length lines)))
    (list rocks goblins elfs (string-length (first lines)) (length lines))))

