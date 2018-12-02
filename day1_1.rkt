#lang racket

(foldl + 0
 (map string->number
      (file->lines "day1_1-input.txt")))