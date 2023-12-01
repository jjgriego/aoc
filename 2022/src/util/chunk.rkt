#lang racket/base

(require racket/list
         racket/stream)

(provide (all-defined-out))

(define (chunk xs group-size)
  (define-values (first-chunk more) (split-at xs group-size))
  (cond
    [(empty? more)
     (stream first-chunk)]
    [else
     (stream-cons first-chunk
                  (chunk more group-size))]))
