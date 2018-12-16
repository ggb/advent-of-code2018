#lang racket

(require threading)

(define (run-instruction inp-a inp-b inp-c regs opc)
  (cond
    [(= 15 opc) (hash-set regs inp-c (+ (hash-ref regs inp-a) (hash-ref regs inp-b)))]           ; addr
    [(=  4 opc) (hash-set regs inp-c (+ (hash-ref regs inp-a) inp-b))]                           ; addi
    [(=  6 opc) (hash-set regs inp-c (* (hash-ref regs inp-a) (hash-ref regs inp-b)))]           ; mulr
    [(=  5 opc) (hash-set regs inp-c (* (hash-ref regs inp-a) inp-b))]                           ; muli
    [(= 11 opc) (hash-set regs inp-c (bitwise-and (hash-ref regs inp-a) (hash-ref regs inp-b)))] ; banr
    [(=  8 opc) (hash-set regs inp-c (bitwise-and (hash-ref regs inp-a) inp-b))]                 ; bani
    [(= 12 opc) (hash-set regs inp-c (bitwise-ior (hash-ref regs inp-a) (hash-ref regs inp-b)))] ; borr
    [(= 10 opc) (hash-set regs inp-c (bitwise-ior (hash-ref regs inp-a) inp-b))]                 ; bori
    [(=  2 opc) (hash-set regs inp-c (hash-ref regs inp-a))]                                     ; setr
    [(=  0 opc) (hash-set regs inp-c inp-a)]                                                     ; seti
    [(=  3 opc) (hash-set regs inp-c (if (> inp-a (hash-ref regs inp-b)) 1 0))]                  ; gtir
    [(=  9 opc) (hash-set regs inp-c (if (> (hash-ref regs inp-a) inp-b) 1 0))]                  ; gtri
    [(=  7 opc) (hash-set regs inp-c (if (> (hash-ref regs inp-a) (hash-ref regs inp-b)) 1 0))]  ; gtrr
    [(=  1 opc) (hash-set regs inp-c (if (= inp-a (hash-ref regs inp-b)) 1 0))]                  ; eqir
    [(= 13 opc) (hash-set regs inp-c (if (= (hash-ref regs inp-a) inp-b) 1 0))]                  ; eqri
    [(= 14 opc) (hash-set regs inp-c (if (= (hash-ref regs inp-a) (hash-ref regs inp-b)) 1 0))])); eqrr

(define (solve-puzzle file-name)
  (~>> (file->lines file-name)
       (map (lambda (x) (map string->number (string-split x))))
       (foldl (lambda (ele acc) (run-instruction (second ele) (third ele) (fourth ele) acc (first ele))) #hash((0 . 1) (1 . 0) (2 . 0) (3 . 0)))))

; (solve-puzzle "day16_input2.txt")