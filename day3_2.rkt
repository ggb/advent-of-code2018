#lang racket

(require threading)

(define (elf-squares offset-x offset-y width height)
  (list->set
   (foldl append (list)
          (for/list ([x (in-range offset-x (+ offset-x width))])
            (for/list ([y (in-range offset-y (+ offset-y height))])
              (cons x y))))))

(define (intersect-helper a b)
  (if (equal? a b)
      (set)
      (set-intersect a b)))

(define (elf-collisions all-squares elf-square)
  (~>> all-squares
       (map (curry intersect-helper elf-square))
       (foldl set-union (set))))

(define (parse-input input-str)
  (~>> (string-split input-str #px"\\@|,|:|x")
       (rest)
       (map (compose string->number string-trim))))

(define (revert-to-dim pair-list)
  (let* ([pairs (set->list (first pair-list))]
         [fst (map car pairs)]
         [scd (map cdr pairs)]
         [min-fst (apply min fst)]
         [min-scd (apply min scd)])
    (format "~a,~a: ~ax~a"
            min-fst
            min-scd
            (add1 (- (apply max fst) min-fst))
            (add1 (- (apply max scd) min-scd)))))
 
(define (solve-puzzle input)
  (let* ([all-squares (~>> input
                          (map parse-input)
                          (map (λ (e) (apply elf-squares e))))]
         [overlap (~>> all-squares
                       (map (curry elf-collisions all-squares))
                       (foldl set-union (set)))]
         [target-dims (~>> all-squares
                           (filter (lambda (e) (set-empty? (set-intersect e overlap))))
                           (revert-to-dim))])
    (first (filter (λ (e) (string-contains? e target-dims)) input))))

(define test-input 
  (list "#1 @ 1,3: 4x4" "#1 @ 3,1: 4x4" "#3 @ 5,5: 2x2"))

(equal? "#3 @ 5,5: 2x2" (solve-puzzle test-input))
