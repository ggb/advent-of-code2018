#lang racket

(provide parse-input solve-puzzle*)

(define (down p)
  (list (first p) (add1 (second p))))

(define (left p)
  (list (sub1 (first p)) (second p)))

(define (right p)
  (list (add1 (first p)) (second p)))

(define (sand? clay water p)
  (and (not (set-member? clay p))
       (not (hash-has-key? water p))))

(define (holds? clay water dir p)
  (and (or (set-member? clay (down p))
           (hash-has-key? water (down p)))
       (or (set-member? clay (dir p))
           (holds? clay water dir (dir p)))))

(define (fill clay water dir p)
  (define water* (if (not (set-member? clay (dir p)))
                     (fill clay water dir p)
                     water))
  (hash-set water* p "~"))

(define (spill clay water max-y dir p)
  (if (and (sand? clay water (dir p))
           (or (set-member? clay (down p))
               (equal? "~" (hash-ref water (down p) ""))))
      (flow clay water max-y (dir p))
      water))

(define (flow clay w max-y p)
  (define _ (println w))
  (define water (hash-set w p "|"))
  (define p1 (println (= max-y (second p))))
  (define p2 (println (sand? clay water p)))
  (define p3 (println (and (holds? clay water left p) (holds? clay water right p))))
  (cond
    [(= max-y (second p)) water]
    [(sand? clay water p) (flow clay water max-y (down p))]
    [(and (holds? clay water left p) (holds? clay water right p)) (fill clay (fill clay water left p) right p)]
    [else (spill clay (spill clay water max-y right p) max-y left p)]))

(define (solve-puzzle* inp)
  (define parsed (parse-input inp))
  (flow (third parsed) (hash) (second parsed) (list 500 (first parsed))))

(define (parse-line line)
  (define splitted (rest (regexp-match #px"(\\w)=(\\d+), (\\w)=(\\d+)\\.\\.(\\d+)" line)))
  (define a (string->number (fourth splitted)))
  (define b (string->number (fifth splitted)))
  (define c (string->number (second splitted)))
  (define r (range a (add1 b)))
  (if (equal? "x" (first splitted))
      (map (curry list c) r)
      (map (lambda (x) (list x c)) r)))

(define (parse-input lines)
  (define point-list (append* (map parse-line lines)))
  (define min-y (apply min (map second point-list)))
  (define max-y (apply max (map second point-list)))
  (list min-y max-y (list->set point-list)))

(define test-input
  (list "x=495, y=2..7"
        "y=7, x=495..501"
        "x=501, y=3..7"
        "x=498, y=2..4"
        "x=506, y=1..2"
        "x=498, y=10..13"
        "x=504, y=10..13"
        "y=13, x=498..504"))