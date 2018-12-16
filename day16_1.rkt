#lang racket

(require threading)

(define (run-instruction inp-a inp-b inp-c regs opc)
  (cond
    [(=  0 opc) (hash-set regs inp-c (+ (hash-ref regs inp-a) (hash-ref regs inp-b)))]           ; addr
    [(=  1 opc) (hash-set regs inp-c (+ (hash-ref regs inp-a) inp-b))]                           ; addi
    [(=  2 opc) (hash-set regs inp-c (* (hash-ref regs inp-a) (hash-ref regs inp-b)))]           ; mulr
    [(=  3 opc) (hash-set regs inp-c (* (hash-ref regs inp-a) inp-b))]                           ; muli
    [(=  4 opc) (hash-set regs inp-c (bitwise-and (hash-ref regs inp-a) (hash-ref regs inp-b)))] ; banr
    [(=  5 opc) (hash-set regs inp-c (bitwise-and (hash-ref regs inp-a) inp-b))]                 ; bani
    [(=  6 opc) (hash-set regs inp-c (bitwise-ior (hash-ref regs inp-a) (hash-ref regs inp-b)))] ; borr
    [(=  7 opc) (hash-set regs inp-c (bitwise-ior (hash-ref regs inp-a) inp-b))]                 ; bori
    [(=  8 opc) (hash-set regs inp-c (hash-ref regs inp-a))]                                     ; setr
    [(=  9 opc) (hash-set regs inp-c inp-a)]                                                     ; seti
    [(= 10 opc) (hash-set regs inp-c (if (> inp-a (hash-ref regs inp-b)) 1 0))]                  ; gtir
    [(= 11 opc) (hash-set regs inp-c (if (> (hash-ref regs inp-a) inp-b) 1 0))]                  ; gtri
    [(= 12 opc) (hash-set regs inp-c (if (> (hash-ref regs inp-a) (hash-ref regs inp-b)) 1 0))]  ; gtrr
    [(= 13 opc) (hash-set regs inp-c (if (= inp-a (hash-ref regs inp-b)) 1 0))]                  ; eqir
    [(= 14 opc) (hash-set regs inp-c (if (= (hash-ref regs inp-a) inp-b) 1 0))]                  ; eqri
    [(= 15 opc) (hash-set regs inp-c (if (= (hash-ref regs inp-a) (hash-ref regs inp-b)) 1 0))])); eqrr

(define (list->hash l)
  (make-immutable-hash (map cons (range (length l)) l)))

(define (behave-like-three inp-a inp-b inp-c before after)
  (let ([before-hash (list->hash before)]
        [after-hash (list->hash after)])
    (~>> (range 16)
         (map (curry run-instruction inp-a inp-b inp-c before-hash))
         (filter (curry equal? after-hash))
         (length))))

(define (to-regs line)
  (map string->number (rest (regexp-match #px"(\\d), (\\d), (\\d), (\\d)" line))))

(define (lines->input l)
  (list (to-regs (first l))
        (map string->number (string-split (second l)))
        (to-regs (third l))))

(define (instruction-sets l is)
  (if (empty? l)
      is
      (instruction-sets (if (> (length l) 3) (drop l 4) '()) (cons (take l 3) is))))

(define (parse-input file-name)
  (map lines->input (instruction-sets (file->lines file-name) (list))))

(define (solve-puzzle parsed-input)
  (~>> parsed-input
       (map (lambda (m) (let ([s (second m)])
                     (behave-like-three (second s) (third s) (fourth s) (first m) (third m)))))
       (filter (curry <= 3))
       (length)))