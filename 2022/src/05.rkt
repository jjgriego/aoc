#lang racket

(parameterize ([current-input-port (open-input-file "../inputs/05")])
  (define stack-lines (drop-right (for/list ([line (stop-before (in-lines) (negate non-empty-string?))])
                                    (for/list ([c line]
                                               [i (in-naturals)]
                                               #:when (= 1 (remainder i 4)))
                                      c))
                                  1))

  (define stacks (for/vector ([idx (in-range (length (first stack-lines)))])
                   (dropf
                    (for/list ([line stack-lines])
                      (list-ref line idx))
                    (lambda (x) (eq? x #\space)))))

  (for ([line (in-lines)])
    (match-define (list count src+1 dst+1)
      (map string->number (string-split line #rx"[^0-9]+" #:trim? #t #:repeat? #t)))

    (define src (sub1 src+1))
    (define dst (sub1 dst+1))

    ;; do the move
    (define-values (moved rest) (split-at (vector-ref stacks src) count))
    (vector-set! stacks src rest)
    (vector-set! stacks dst (append moved (vector-ref stacks dst)))
    #;(vector-set! stacks dst (append (reverse moved) (vector-ref stacks dst))))

  (list->string (for/list ([stack stacks])
                  (car stack))))
