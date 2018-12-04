#lang racket

(require threading)

(provide create-sleep-list test-input)

(define (create-sleep-list sleep-hash current-guard sleep-store input)
  (if (empty? input)
      sleep-hash
      (let* ([ele (first input)]
             [guard? (regexp-match #px"#\\d{1,4}" ele)]
             [asleep? (regexp-match  #rx"asleep" ele)]
             [wake? (regexp-match  #rx"wakes up" ele)]
             [minutes (string->number (second (regexp-match #px"(\\d\\d)\\]" ele)))])
        (cond
          [guard?  (create-sleep-list
                    (if (hash-has-key? sleep-hash (first guard?))
                        sleep-hash
                        (hash-set sleep-hash (first guard?) (list)))
                    (first guard?)
                    #f
                    (rest input))]
          [asleep? (create-sleep-list
                    sleep-hash
                    current-guard
                    minutes
                    (rest input))]
          [wake?   (create-sleep-list
                    (hash-update sleep-hash current-guard (λ (v) (cons (cons sleep-store minutes) v)))
                    current-guard
                    #f
                    (rest input))]))))

(define (calc-sleep-times input)
  (hash-map input (λ (k v)
                    (cons k (foldl (λ (pair acc) (+ acc (- (cdr pair) (car pair)))) 0 v)))))

(define (solve-puzzle input)
  (let* ([sleep-hash (~>> (sort input string<?)
                          (create-sleep-list (hash) #f #f))]
         [max-sleep-time (~>> sleep-hash
                              (calc-sleep-times)
                              (argmax cdr))]
         [t (println max-sleep-time)])
    (~>> (hash-ref sleep-hash (car max-sleep-time))
         (map (λ (v) (range (car v) (cdr v))))
         (flatten)
         (group-by identity)
         (map (λ (v) (cons (first v) (length v))))
         (argmax cdr)
         (car)
         (* (string->number (string-replace (car max-sleep-time) "#" ""))))))
        
(define test-input
  (list "[1518-11-05 00:03] Guard #99 begins shift"
        "[1518-11-05 00:45] falls asleep"
        "[1518-11-05 00:55] wakes up"
        "[1518-11-04 00:02] Guard #99 begins shift"
        "[1518-11-04 00:36] falls asleep"
        "[1518-11-03 00:24] falls asleep"
        "[1518-11-03 00:29] wakes up"
        "[1518-11-01 00:00] Guard #10 begins shift"
        "[1518-11-01 00:05] falls asleep"
        "[1518-11-01 00:25] wakes up"
        "[1518-11-01 00:30] falls asleep"
        "[1518-11-01 00:55] wakes up"
        "[1518-11-01 23:58] Guard #99 begins shift"
        "[1518-11-02 00:40] falls asleep"
        "[1518-11-02 00:50] wakes up"
        "[1518-11-03 00:05] Guard #10 begins shift"
        "[1518-11-04 00:46] wakes up"
        ))

; (solve-puzzle (file->lines "day4_input.txt"))