#lang racket

(define elve-totals
  (for/list ([group (let loop ([lines (port->lines (open-input-file "../inputs/01"))]
                               [groups '()])
                      (define-values (group more) (splitf-at lines (lambda (x)
                                                                     (not
                                                                      (equal? x "")))))
                      (match more
                        ['() (reverse groups)]
                        [(cons "" still-more) (loop still-more (cons group groups))]))])
    (foldl + 0 (map string->number group))))

#;(argmax identity elve-totals)

#;(apply + (take (sort elve-totals >) 3))


