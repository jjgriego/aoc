#lang racket

(define (distinct? xs)
  (not (check-duplicates xs eq?)))

(define chars (string->list (port->string (open-input-file "../inputs/06"))))

(for/first ([idx (in-range (- (length chars) 13))]
            #:when (distinct? (take (drop chars idx) 14)))
  (+ idx 14))


