#lang racket

(struct cart (pos direction turn))

(define (split-line carts track ln line)
  (define splitted (drop (drop-right (string-split line "") 1) 1))
  (foldl
   (lambda (ele acc)
     (let ([c (car acc)]
           [t (cdr acc)]
           [e (car ele)]
           [i [cdr ele]])
       (cond
         [(equal? e " ") acc]
         [(equal? e "^") (cons (cons (cart (cons i ln) 'up 'left) c) (hash-set t (cons i ln) "|"))]
         [(equal? e "v") (cons (cons (cart (cons i ln) 'down 'left) c) (hash-set t (cons i ln) "|"))]
         [(equal? e "<") (cons (cons (cart (cons i ln) 'left 'left) c) (hash-set t (cons i ln) "-"))]
         [(equal? e ">") (cons (cons (cart (cons i ln) 'right 'left) c) (hash-set t (cons i ln) "-"))]
         [else (cons c(hash-set t (cons i ln) e))])))
   (cons carts track)
   (map cons splitted (range (length splitted)))))
  

(define test-input
  (list "/->-\\        "
        "|   |  /----\\"
        "| /-+--+-\\  |"
        "| | |  | v  |"
        "\\-+-/  \\-+--/"
        "  \\------/   "))