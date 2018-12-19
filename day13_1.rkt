#lang racket

(struct cart (pos direction turn))

(define (move-up p)
  (cons (car p) (sub1 (cdr p))))

(define (move-down p)
  (cons (car p) (add1 (cdr p))))

(define (move-left p)
  (cons (sub1 (car p)) (cdr p)))

(define (move-right p)
  (cons (add1 (car p)) (cdr p)))

(define (move-cart track ct)
  (define position (hash-ref track (cart-pos ct)))
  (define direction (cart-direction ct))
  (define turn (cart-turn ct))
  (cond
    ;
    [(and (equal? position "|") (equal? direction 'up)) (struct-copy cart ct [pos (move-up position)])]
    [(and (equal? position "|") (equal? direction 'down)) (struct-copy cart ct [pos (move-down position)])]
    [(and (equal? position "-") (equal? direction 'left)) (struct-copy cart ct [pos (move-left position)])]
    [(and (equal? position "-") (equal? direction 'right)) (struct-copy cart ct [pos (move-right position)])]
    ;
    [(and (equal? position "/") (equal? direction 'up)) (cart (move-right position) 'right turn)]
    [(and (equal? position "/") (equal? direction 'down)) (cart (move-left position) 'left turn)]
    [(and (equal? position "/") (equal? direction 'left)) (cart (move-down position) 'down turn)]
    [(and (equal? position "/") (equal? direction 'right)) (cart (move-up position) 'up turn)]
    ;
    [(and (equal? position "\\") (equal? direction 'up)) (cart (move-left position) 'left turn)]
    [(and (equal? position "\\") (equal? direction 'down)) (cart (move-right position) 'right turn)]
    [(and (equal? position "\\") (equal? direction 'left)) (cart (move-up position) 'up turn)]
    [(and (equal? position "\\") (equal? direction 'right)) (cart (move-down position) 'down turn)]
    ;
    [(and (equal? turn 'left) (equal? direction 'up)) (cart (move-left position) 'left 'straight)]
    [(and (equal? turn 'left) (equal? direction 'down)) (cart (move-right position) 'right 'straight)]
    [(and (equal? turn 'left) (equal? direction 'left)) (cart (move-down position) 'down 'straight)]
    [(and (equal? turn 'left) (equal? direction 'right)) (cart (move-up position) 'up 'straight)]
    ;
    [(and (equal? turn 'straight) (equal? direction 'up)) (cart (move-up position) 'up 'right)]
    [(and (equal? turn 'straight) (equal? direction 'down)) (cart (move-down position) 'down 'right)]
    [(and (equal? turn 'straight) (equal? direction 'left)) (cart (move-left position) 'left 'right)]
    [(and (equal? turn 'straight) (equal? direction 'right)) (cart (move-right position) 'right 'right)]
    ;
    [(and (equal? turn 'right) (equal? direction 'up)) (cart (move-left position) 'right 'left)]
    [(and (equal? turn 'right) (equal? direction 'down)) (cart (move-right position) 'left 'left)]
    [(and (equal? turn 'right) (equal? direction 'left)) (cart (move-down position) 'up 'left)]
    [(and (equal? turn 'right) (equal? direction 'right)) (cart (move-up position) 'down 'left)]))

(define (crash? ct carts)
  '())

(define (solve-puzzle* carts track crashs)
  (if (not (empty? crashs))
      crashs
      (solve-puzzle* (map (curry move-cart track) carts) track crashs)))

(define (solve-puzzle carts track)
  (solve-puzzle* carts track '()))

(define (split-line carts track line ln)
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

(define (parse-input inp)
  (foldl
   (lambda (ele acc)
     (split-line (car acc) (cdr acc) (car ele) (cdr ele)))
   (cons '() (hash))
   (map cons inp (range (length inp)))))

(define test-input
  (list "/->-\\        "
        "|   |  /----\\"
        "| /-+--+-\\  |"
        "| | |  | v  |"
        "\\-+-/  \\-+--/"
        "  \\------/   "))