#lang racket

(provide solve-puzzle)

(define (update-list current new-n l)
  (let* ([index (index-of l current)]
         [new-index (modulo (+ index 2) (length l))])
    (let-values ([(front tail) (split-at l new-index)])
      (append front (cons new-n tail)))))

(define (deleteNth n l)
  (cond
    [(= n 0) (rest l)]
    [(< n (length l)) (append (take l n) (rest (drop l n)))]
    [else l]))

(define (calc-score n l)
  (let* ([index (index-of l n)]
         [nine-left (modulo (- index 9) (length l))]
         [new-current (list-ref l (modulo (- index 8) (length l)))]
         [score (+ (list-ref l index) (list-ref l nine-left))]
         [updated-list (if (= index (max index nine-left))
                           (deleteNth nine-left (deleteNth index l))
                           (deleteNth index (deleteNth nine-left l)))])
    (values new-current score updated-list)))

(define (update-player player-n current)
  (add1 (modulo current player-n)))

(define (create-score-table player-n)
  (make-immutable-hash (map (lambda (i) (cons i 0)) (range 1 (add1 player-n)))))

(define (run-game player-n marble-count)
  (foldl (lambda (ele acc)
           (let-values ([(pos score player old-list) (apply values acc)])
             (let ([current-player (update-player player-n player)])
               (if (= 0 (modulo ele 23))
                   (let-values ([(new-current new-score updated-list) (calc-score ele (update-list pos ele old-list))])
                     (list new-current (hash-update score current-player (curry + new-score)) current-player updated-list))
                   (list ele score current-player (update-list pos ele old-list))))))
         ; current-pos / score / player / list
         (list 0 (create-score-table player-n) 0 '(0))
         (range 1 (add1 marble-count))))

(define (solve-puzzle player-n marble-value)
  (argmax cdr (hash->list (second (run-game player-n marble-value)))))

(=   32 (cdr (solve-puzzle 9 25)))
(= 8317 (cdr (solve-puzzle 10 1618)))
(= 2764 (cdr (solve-puzzle 17 1104)))

;(time (solve-puzzle 459 71320))