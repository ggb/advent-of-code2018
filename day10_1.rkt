#lang racket/gui

(require 2htdp/image)
(require threading)

(define canvas-width 600)
(define canvas-height 600)

(define (move-point p)
  (let ([position (car p)]
        [velocity (cdr p)])
    (cons (cons (+ (car position) (car velocity))
                (+ (cdr position) (cdr velocity)))
          velocity)))

(define (position-at-i i points)
  (map (lambda (p)
         (cons (cons (+ (caar p) (* i (cadr p)))
                     (+ (cdar p) (* i (cddr p))))
               (cdr p)))
       points))

(define (parse-input inp)
  (~>> inp
       (map (curry regexp-match #px"position=<\\s*?(-?\\d{1,6}),\\s*?(-?\\d{1,6})> velocity=<\\s*?(-?\\d{1,6}),\\s*?(-?\\d{1,6})>"))
       (map (lambda (l) (cons (cons (string->number (second l)) (string->number (third l)))
                              (cons (string->number (fourth l)) (string->number (fifth l))))))))

(define test-input
  (parse-input (list "position=< 9,  1> velocity=< 0,  2>"
        "position=< 7,  0> velocity=<-1,  0>"
        "position=< 3, -2> velocity=<-1,  1>"
        "position=< 6, 10> velocity=<-2, -1>"
        "position=< 2, -4> velocity=< 2,  2>"
        "position=<-6, 10> velocity=< 2, -2>"
        "position=< 1,  8> velocity=< 1, -1>"
        "position=< 1,  7> velocity=< 1,  0>"
        "position=<-3, 11> velocity=< 1, -2>"
        "position=< 7,  6> velocity=<-1, -1>"
        "position=<-2,  3> velocity=< 1,  0>"
        "position=<-4,  3> velocity=< 2,  0>"
        "position=<10, -3> velocity=<-1,  1>"
        "position=< 5, 11> velocity=< 1, -2>"
        "position=< 4,  7> velocity=< 0, -1>"
        "position=< 8, -2> velocity=< 0,  1>"
        "position=<15,  0> velocity=<-2,  0>"
        "position=< 1,  6> velocity=< 1,  0>"
        "position=< 8,  9> velocity=< 0, -1>"
        "position=< 3,  3> velocity=<-1,  1>"
        "position=< 0,  5> velocity=< 0, -1>"
        "position=<-2,  2> velocity=< 2,  0>"
        "position=< 5, -2> velocity=< 1,  2>"
        "position=< 1,  4> velocity=< 2,  1>"
        "position=<-2,  7> velocity=< 2, -2>"
        "position=< 3,  6> velocity=<-1, -1>"
        "position=< 5,  0> velocity=< 1,  0>"
        "position=<-6,  0> velocity=< 2,  0>"
        "position=< 5,  9> velocity=< 1, -2>"
        "position=<14,  7> velocity=<-2,  0>"
        "position=<-3,  6> velocity=< 2, -1>")))

(define image
  (circle 1 "solid" "blue"))
                     
(define (image->bitmap image)  
  (let* ([width (add1 (image-width image))]
         [height (add1 (image-height image))]
         [bm (make-bitmap width height)]
         [dc (send bm make-dc)])
    (send dc clear)
    (send image draw dc 0 0 0 0 width height 0 0 #f)
    bm))

(define (solve-puzzle inp)
  (define frame
    (new frame% [label "Frame"] [width canvas-width] [height canvas-height]))

  (define canvas
    (new canvas% [parent frame]))

  (define dc
    (send canvas get-dc))

  (send frame show #t)
  (sleep/yield 1)

  (thread
   (lambda ()
     (let ([points (box inp)]
           [counter (box 0)])
       (for ((i (range 10300 10370)))
         (begin
           (send dc clear)
           (for ((p (unbox points)))
             (send dc draw-bitmap
                   (image->bitmap image)
                   (- (* 3 (caar p)) 180) (- (* 3 (cdar p)) 80)))
           (sleep 0.01)
           ;(set-box! counter (+ 1 (unbox counter)))
           (set-box! points (map move-point (unbox points)))))))))


;(solve-puzzle test-input)
(solve-puzzle (position-at-i 10300 (parse-input (file->lines "day10_input.txt"))))