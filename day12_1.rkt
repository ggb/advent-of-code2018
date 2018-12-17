#lang racket

(require threading)

(provide solve-puzzle)

(define (current-position state pos)
  (string-join (map (lambda (x) (hash-ref state x ".")) (range (- pos 2) (+ pos 3))) ""))

(define (to-initial-state line)
  (let ([l (drop (drop-right (string-split (string-replace line "initial state: " "") "") 1) 1)])
    (make-immutable-hash (map cons (range (length l)) l))))
       
(define (to-mapping lines)
  (make-immutable-hash (map (lambda (l) (string-split l " => ")) lines)))

(define (parse-input inp)
  (values (to-initial-state (first inp)) (to-mapping (drop inp 2))))

(define (get-updates state mapping min max)
  (foldl (lambda (ele acc)
             (if (hash-has-key? mapping (cdr ele))
                 (hash-set acc (car ele) (first (hash-ref mapping (cdr ele))))
                 acc))
           (hash)
           (map (lambda (i)
                  (cons i (current-position state i)))
                (range min max))))

(define (print-state state)
  (println (string-join (map (lambda (x) (if (hash-has-key? state x) (hash-ref state x) ".")) (range -5 120)) "")))

(define (solve-puzzle n viz? inp)
  (let-values ([(state mapping) (parse-input inp)])
    (~>> (range n)
         (foldr (lambda (ele acc)
                  (begin
                    (and viz? (print-state (third acc)))
                    (list
                     (first acc)
                     (+ (second acc) 2)
                     (get-updates (third acc) mapping (first acc) (second acc)))))
                (list -20 (+ 2 (hash-count state)) state))
         (third)
         (hash->list)
         (filter (lambda (e) (equal? "#" (cdr e))))
         (map car)
         (apply +))))
                

(define test-input
  (list "initial state: #..#.#..##......###...###"
        ""
        "...## => #"
        "..#.. => #"
        ".#... => #"
        ".#.#. => #"
        ".#.## => #"
        ".##.. => #"
        ".#### => #"
        "#.#.# => #"
        "#.### => #"
        "##.#. => #"
        "##.## => #"
        "###.. => #"
        "###.# => #"
        "####. => #"))

(= 325 (solve-puzzle 20 #t test-input))

; (solve-puzzle 20 #t (file->lines "day12_input.txt"))