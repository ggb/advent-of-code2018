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
  (define position (cart-pos ct))
  (define content (hash-ref track (cart-pos ct)))
  (define direction (cart-direction ct))
  (define turn (cart-turn ct))
  (cond
    ;
    [(and (equal? content "|") (equal? direction 'up)) (struct-copy cart ct [pos (move-up position)])]
    [(and (equal? content "|") (equal? direction 'down)) (struct-copy cart ct [pos (move-down position)])]
    [(and (equal? content "-") (equal? direction 'left)) (struct-copy cart ct [pos (move-left position)])]
    [(and (equal? content "-") (equal? direction 'right)) (struct-copy cart ct [pos (move-right position)])]
    ;
    [(and (equal? content "/") (equal? direction 'up)) (cart (move-right position) 'right turn)]
    [(and (equal? content "/") (equal? direction 'down)) (cart (move-left position) 'left turn)]
    [(and (equal? content "/") (equal? direction 'left)) (cart (move-down position) 'down turn)]
    [(and (equal? content "/") (equal? direction 'right)) (cart (move-up position) 'up turn)]
    ;
    [(and (equal? content "\\") (equal? direction 'up)) (cart (move-left position) 'left turn)]
    [(and (equal? content "\\") (equal? direction 'down)) (cart (move-right position) 'right turn)]
    [(and (equal? content "\\") (equal? direction 'left)) (cart (move-up position) 'up turn)]
    [(and (equal? content "\\") (equal? direction 'right)) (cart (move-down position) 'down turn)]
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
    [(and (equal? turn 'right) (equal? direction 'up)) (cart (move-right position) 'right 'left)]
    [(and (equal? turn 'right) (equal? direction 'down)) (cart (move-left position) 'left 'left)]
    [(and (equal? turn 'right) (equal? direction 'left)) (cart (move-up position) 'up 'left)]
    [(and (equal? turn 'right) (equal? direction 'right)) (cart (move-down position) 'down 'left)]))

(define (crash? carts ct)
  (foldl (lambda (ele acc)
           (cond
             [(equal? ct ele) acc]
             [(equal? (cart-pos ct) (cart-pos ele)) (cons (cart-pos ct) acc)]
             [else acc]))
         '()
         carts))

(define (check-crashs? carts)
  (filter (negate empty?) (map (curry crash? carts) carts)))

(define (solve-puzzle* iterations carts track crashs)
  (if (or false ;(> iterations 20)
          (not (empty? crashs)))
      (begin
        (println iterations)
      crashs)
      (let* ([updated-carts (map (curry move-cart track) carts)]
             [updated-crashs (check-crashs? updated-carts)])
        (solve-puzzle* (add1 iterations) updated-carts track updated-crashs))))

(define (solve-puzzle carts track)
  (solve-puzzle* 0 carts track '()))

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

; (define t (parse-input (file->lines "day13_input.txt")))
; (solve-puzzle (car t) (cdr t))