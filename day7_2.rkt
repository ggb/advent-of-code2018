#lang racket

(require threading)
(require "day7_1.rkt")

(define (worker letter val)
  (λ ()
    (if (= 1 val)
        letter
        (worker letter (sub1 val)))))

(define (str->val val)
  (- (char->integer (string-ref val 0)) 4))

(define (create-worker val)
  (worker val (str->val val)))

(define (available? pairs seen target)
  (empty?
   (set-subtract
    (map car (filter (λ (x)
                       (equal? target (cdr x)))
                     pairs))
    seen)))

(define (done-diff pairs new-done res open)
  (append
   (filter
    (curry available? pairs new-done)
    (map cdr (find-next pairs res)))
   open))

(define (work! pairs open done worker-list)
  (foldr
   (λ (worker acc)
     (let ([res (if (equal? 'idle worker) 'idle (worker))]
           [current-wl (first acc)]
           [current-open (second acc)]
           [current-done (third acc)])
       (cond
         [(procedure? res) (list (cons res current-wl) current-open current-done)]
         [(string? res) (let ([new-open (done-diff pairs (cons res current-done) res current-open)])
                          (if (empty? new-open)
                              (list (cons 'idle current-wl) new-open (cons res current-done))
                              (list (cons (create-worker (first new-open)) current-wl) (rest new-open) (cons res current-done))))]
         [(equal? res 'idle) (if (empty? current-open)
                                 (list (cons 'idle current-wl) current-open current-done)
                                 (list (cons (create-worker (first current-open)) current-wl) (rest current-open) current-done))])))
   (list '() open done)
   worker-list))

(define (solve counter pairs worker-list open done)
  (if (and (empty? open)
           (andmap (λ (x)
                     (equal? x 'idle))
                   worker-list))
      (sub1 counter)
      (let* ([step (work! pairs open done worker-list)]
             [current-wl (first step)]
             [current-open (second step)]
             [current-done (third step)])
        (solve (add1 counter) pairs current-wl current-open current-done))))

(define (solve-puzzle input)
  (let* ([pairs (parse-input input)]
         [start (find-start pairs)])
    (solve -1 pairs (list 'idle 'idle 'idle 'idle 'idle) start (list))))