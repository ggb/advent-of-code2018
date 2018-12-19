#lang racket

(require threading)

(define (run-instruction opc inp-a inp-b inp-c regs)
  (cond
    [(equal? "addr" opc) (hash-set regs inp-c (+ (hash-ref regs inp-a) (hash-ref regs inp-b)))]           ; addr
    [(equal? "addi" opc) (hash-set regs inp-c (+ (hash-ref regs inp-a) inp-b))]                           ; addi
    [(equal? "mulr" opc) (hash-set regs inp-c (* (hash-ref regs inp-a) (hash-ref regs inp-b)))]           ; mulr
    [(equal? "muli" opc) (hash-set regs inp-c (* (hash-ref regs inp-a) inp-b))]                           ; muli
    [(equal? "banr" opc) (hash-set regs inp-c (bitwise-and (hash-ref regs inp-a) (hash-ref regs inp-b)))] ; banr
    [(equal? "bani" opc) (hash-set regs inp-c (bitwise-and (hash-ref regs inp-a) inp-b))]                 ; bani
    [(equal? "borr" opc) (hash-set regs inp-c (bitwise-ior (hash-ref regs inp-a) (hash-ref regs inp-b)))] ; borr
    [(equal? "bori" opc) (hash-set regs inp-c (bitwise-ior (hash-ref regs inp-a) inp-b))]                 ; bori
    [(equal? "setr" opc) (hash-set regs inp-c (hash-ref regs inp-a))]                                     ; setr
    [(equal? "seti" opc) (hash-set regs inp-c inp-a)]                                                     ; seti
    [(equal? "gtir" opc) (hash-set regs inp-c (if (> inp-a (hash-ref regs inp-b)) 1 0))]                  ; gtir
    [(equal? "gtri" opc) (hash-set regs inp-c (if (> (hash-ref regs inp-a) inp-b) 1 0))]                  ; gtri
    [(equal? "gtrr" opc) (hash-set regs inp-c (if (> (hash-ref regs inp-a) (hash-ref regs inp-b)) 1 0))]  ; gtrr
    [(equal? "eqir" opc) (hash-set regs inp-c (if (= inp-a (hash-ref regs inp-b)) 1 0))]                  ; eqir
    [(equal? "eqri" opc) (hash-set regs inp-c (if (= (hash-ref regs inp-a) inp-b) 1 0))]                  ; eqri
    [(equal? "eqrr" opc) (hash-set regs inp-c (if (= (hash-ref regs inp-a) (hash-ref regs inp-b)) 1 0))])); eqrr

(define (increment-ip ip registers)
  (hash-update registers ip add1))

(define (solve-puzzle* iter-count ip instructions registers)
  ;(define _ (println registers))
  (if (>= (hash-ref registers ip) (vector-length instructions))
      (begin
        (println iter-count)
        registers)
      (let ([inst (vector-ref instructions (hash-ref registers ip))])
        (solve-puzzle*
         (add1 iter-count)
         ip
         instructions
         (~>> registers
              (run-instruction (first inst) (second inst) (third inst) (fourth inst))
              (increment-ip ip))))))

(define (solve-puzzle inp)
  (define registers #hash((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0)))
  (solve-puzzle* 0 (car inp) (cdr inp) registers))

(define (get-instruction-pointer inp)
  (string->number (second (string-split (first inp)))))

(define (parse-instruction line)
  (define splitted (string-split line))
  (cons (first splitted) (map string->number (rest splitted))))

(define (parse-instructions inp)
  (apply vector-immutable (map parse-instruction (rest inp))))

(define (parse-input inp)
  (cons (get-instruction-pointer inp) (parse-instructions inp)))

(define test-input
  (list "#ip 0"
        "seti 5 0 1"
        "seti 6 0 2"
        "addi 0 1 0"
        "addr 1 2 3"
        "setr 1 0 0"
        "seti 8 0 4"
        "seti 9 0 5"))

;(solve-puzzle (parse-input (file->lines "day19_input.txt")))