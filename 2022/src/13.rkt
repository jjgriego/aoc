#lang racket

(require "util/chunk.rkt")

(define (string-sexprify s)
  (list->string
   (for/list ([c s])
     (match c
       [#\[ #\(]
       [#\] #\)]
       [#\, #\space]
       [_ c]))))


(define input (port->list
               read
               (open-input-string
                (string-sexprify
                 (port->string
                  (open-input-file "../inputs/13"))))))

(define (packet-order l r)
  (match* (l r)
    [((? integer? n) (? integer? m))
     (- n m)]
    [((? integer? n) (? list? r))
     (packet-order (list n) r)]
    [((? list? l) (? integer? m))
     (packet-order l (list m))]
    [('() '()) 0]
    [('() (? cons?)) -1]
    [((? cons?) '()) 1]
    [((cons l ls) (cons r rs))
     (define x (packet-order l r))
     (cond
       [(zero? x) (packet-order ls rs)]
       [else x])]))

(for/sum ([pair (chunk input 2)]
          [idx-1 (in-naturals)])
  (define idx (add1 idx-1))
  (define order (packet-order (first pair) (second pair)))
  (if (negative? order)
      idx
      0))

(define divider-1 '((2)))
(define divider-2 '((6)))

(define sorted (sort (list* '((2)) '((6)) input)
                     (lambda (l r)
                       (negative? (packet-order l r)))))
(* (add1 (index-of sorted divider-1))
   (add1 (index-of sorted divider-2)))



