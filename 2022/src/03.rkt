#lang racket

(require "util/chunk.rkt")

(module+ test
  (require rackunit))



(define (char-priority c)
  (cond
    [(char-upper-case? c)
     (+ 27 (- (char->integer c)
              (char->integer #\A)))]
    [(char-lower-case? c)
     (+ 1 (- (char->integer c )
             (char->integer #\a)))]))

(module+ test
  (check-equal? (char-priority #\a) 1)
  (check-equal? (char-priority #\m) 13)
  (check-equal? (char-priority #\z) 26)
  (check-equal? (char-priority #\A) 27)
  (check-equal? (char-priority #\Z) 52))

#;(for/sum ([line (in-lines (open-input-file "../inputs/03"))])
  (define chars (string->list line))
  (define-values (left right) (split-at chars (/ (length chars) 2)))
  (define common (set-intersect left right))
  (match common
    [(list x)
     (char-priority x)]))

(for/sum ([group (chunk (port->lines (open-input-file "../inputs/03")) 3)])
  (define chars (map string->list group))
  (define common (apply set-intersect chars))
  (match common
    [(list x)
     (char-priority x)]))

