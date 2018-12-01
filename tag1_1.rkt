#lang racket

(foldl + 0
 (map string->number
      (file->lines "tag1_1-input.txt")))