#lang racket

(require threading)
(provide elf-loop)

; reg-a: Register 1
; reg-b: Register 3
; reg-c: Register 4
(define (elf-loop reg-a reg-b)
  (let* ([reg-c (bitwise-and reg-b 255)]            ; line 08
         [new-reg-a (~>> (+ reg-a reg-c)            ; line 09
                         (bitwise-and 16777215)     ; line 10
                         (* 65899)                  ; line 11
                         (bitwise-and 16777215))])  ; line 12
    (if (< reg-b 256)                               ; line 13
        new-reg-a
        (elf-loop new-reg-a (quotient reg-b 256)))))

(define (solve-puzzle inp)
  (elf-loop inp 65536))

; Look at instruction 07 for input value:
(solve-puzzle 10905776) 