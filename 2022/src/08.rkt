#lang racket

(require "util/grid.rkt")

(define lines (port->lines (open-input-file "../inputs/08")))

(define field-w (string-length (car lines)))
(define field-h (length lines))

(define trees (make-grid field-w field-h))
(for ([line lines]
      [j (in-naturals)])
  (for ([c line]
        [i (in-naturals)])
    (grid-set! trees i j (string->number (string c)))))

(define (is-visible-in-direction? height i j di dj)
  (define (step height i j)
    (or (< i 0) (< j 0)
        (<= field-w i) (<= field-h j)
        (and (not (>= (grid-ref trees i j) height))
             (step height (+ i di) (+ j dj)))))

  (step height (+ i di) (+ j dj)))

(define visibility (grid-imap (lambda (v i j)
                                (or
                                 (is-visible-in-direction? v i j 1 0)
                                 (is-visible-in-direction? v i j -1 0)
                                 (is-visible-in-direction? v i j 0 1)
                                 (is-visible-in-direction? v i j 0 -1)))
                              trees))

(vector-count identity (grid-data visibility))

(define (scenic-score/dir height i j di dj)
  (define (step accum height i j)
    (cond
      [(or (< i 0) (< j 0)
           (<= field-w i) (<= field-h j))
       accum]
      [(>= (grid-ref trees i j) height)
       (add1 accum)]
      [else
       (step (add1 accum) height
             (+ i di) (+ j dj))]))

  (step 0 height (+ i di) (+ j dj)))

(define scenic-scores (grid-imap (lambda (v i j)
                                   (*
                                    (scenic-score/dir v i j 1 0)
                                    (scenic-score/dir v i j -1 0)
                                    (scenic-score/dir v i j 0 1)
                                    (scenic-score/dir v i j 0 -1)))
                                 trees))

(argmax identity (vector->list (grid-data scenic-scores)))

