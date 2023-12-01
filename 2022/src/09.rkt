#lang racket

(require "util/grid.rkt")

;; we define how the tail moves in response to the head with this table
;; which is indexed (more or less) by their relative position
;; (because i am too lazy to write code that produces this table)

;; the tail should _never_ be more than 2 units away from the head along any
;; axis, since each rope segment moves at most one unit on each axis at a time

(define moves
  (grid
   (vector
    ;;    -2           -1           0            1          2
    (cons -1 -1) (cons -1 -1) (cons 0 -1) (cons 1 -1) (cons 1 -1) ;; -2
    (cons -1 -1) (cons  0  0) (cons 0  0) (cons 0 0)  (cons 1 -1) ;; -1
    (cons -1  0) (cons  0  0) (cons 0  0) (cons 0 0)  (cons 1  0) ;; 0
    (cons -1  1) (cons  0  0) (cons 0  0) (cons 0 0)  (cons 1  1) ;; 1
    (cons -1  1) (cons -1  1) (cons 0  1) (cons 1 1)  (cons 1  1));; 2
   5 5))

(define (tail-follow head tail)
  (define di (- (car head) (car tail)))
  (define dj (- (cdr head) (cdr tail)))
  (unless (and (<= -2 di 2) (<= -2 dj 2))
    (error 'new-tail "tail too far"))
  (match-define (cons move-i move-j) (grid-ref moves (+ 2 di) (+ 2 dj)))
  (cons (+ move-i (car tail))
        (+ move-j (cdr tail))))

;; head is already moved, tails are not
(define (list-follow head tails)
  (match tails
    ['() tails]
    [(cons next-tail more)
     (define next-tail* (tail-follow head next-tail))
     (cons next-tail* (list-follow next-tail* more))]))

;; rope is non-empty
(define (step rope di dj)
  (define head (car rope))
  (define head* (cons (+ (car head) di)
                      (+ (cdr head) dj)))
  (define tail* (list-follow head (cdr rope)))
  (cons head* tail*))

(define (go rope-length)
  (define visited (mutable-set))
  (for/fold ([rope (make-list rope-length (cons 0 0))])
            ([line (in-lines (open-input-file "../inputs/09"))])
    (match-define (list direction count-str) (string-split line))
    (define count (string->number count-str))
    (define-values (di dj)
      (match direction
        ["R" (values 1 0)]
        ["L" (values -1 0)]
        ["U" (values 0 -1)]
        ["D" (values 0 1)]))
    (for/fold ([rope rope])
              ([_ (in-range count)])
      (set-add! visited (last rope))
      (step rope di dj)))
  (set-count visited))

(go 2)
(go 10)
