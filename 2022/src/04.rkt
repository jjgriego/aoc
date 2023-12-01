#lang racket

(for/sum ([line (in-lines (open-input-file "../inputs/04"))])
  (match-define (list a b c d)
    (map string->number (string-split line #rx"[^0-9]")))
  (cond
    [(or (<= a c b)
         (<= a d b)
         (<= c b d)
         (<= c a d))
     1]
    [else 0])
  #;(cond
    [(or (and (<= a c) (<= d b))
         (and (<= c a) (<= b d)))
     1]
    [else 0]))
