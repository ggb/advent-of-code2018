#lang racket

(struct units (count hit-points weaknesses immunities damage dtype initiative)
  #:transparent)

(define (parse-weaknesses inp)
  (define parsed (append (string-split inp "; ") (list "" "")))
  (define weaknesses (string-replace (if (string-contains? (first parsed) "weak") (first parsed) (second parsed)) "weak to " ""))
  (define immunities (string-replace (if (string-contains? (first parsed) "immune") (first parsed) (second parsed)) "immune to " ""))
  (cons (string-split weaknesses ", ") (string-split immunities ", ")))

(define (parse-line l)
  (define parsed (regexp-match #px"(\\d+) units each with (\\d+) hit points \\((.*)\\) with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)" l))
  (define weaknesses-and-immunities (parse-weaknesses (fourth parsed)))
  (units (string->number (second parsed))
         (string->number (third parsed))
         (car weaknesses-and-immunities)
         (cdr weaknesses-and-immunities)
         (string->number (fifth parsed))
         (sixth parsed)
         (string->number (seventh parsed))))

(define (parse-lines lines)
  (map parse-line (rest lines)))

(define (parse-input inp)
  (define split-pos (index-of inp ""))
  (define immune-lines (take inp split-pos))
  (define infection-lines (drop inp (add1 split-pos)))
  (cons (parse-lines immune-lines) (parse-lines infection-lines)))

(define test-input
  (list "Immune System:"
        "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2"
        "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3"
        ""
        "Infection:"
        "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1"
        "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"))