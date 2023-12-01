#lang racket

(provide (all-defined-out))

(struct grid (w h data) #:transparent)

(define (make-grid w h [v 0])
  (grid w h (make-vector (* w h) v)))

(define (grid-map f g)
  (grid (grid-w g)
        (grid-h g)
        (vector-map f (grid-data g))))

(define (grid-copy g)
  (define g* (make-grid (grid-w g) (grid-h g)))
  (grid-copy! g g* 0 0 0 0 (grid-w g) (grid-h g))
  g*)

(define (grid-copy! src dst x0 y0 x1 y1 w h)
  ;; it might be possible to copy everything at once, if
  ;; we are copying full rows from both grids
  (cond
    [(and (= w (grid-w src) (grid-w dst))
          (= x0 x1 0))
     (define src-start (grid-index-at src 0 y0))
     (vector-copy! (grid-data dst)
                   (grid-index-at dst 0 y1)
                   (grid-data src)
                   src-start
                   (+ src-start (* w h)))]
    [else
     ;; otherwise try the slow path
     (for* ([dy (in-range h)])
       (define y (+ y0 dy))
       (define src-start (grid-index-at src x0 y))
       (vector-copy! (grid-data dst)
                     (grid-index-at dst x1 (+ y1 dy))
                     (grid-data src)
                     src-start
                     (+ src-start w)))]))

(define (in-grid-coordinates g)
  (for*/stream ([j (in-range (grid-h g))]
                [i (in-range (grid-w g))])
    (cons i j)))

(define (grid-imap f g)
  (grid
   (grid-w g)
   (grid-h g)
   (for/vector ([c (in-grid-coordinates g)])
     (define-values (i j) (values (car c) (cdr c)))
     (define v (grid-ref g i j))
     (f v i j))))

(define (grid-valid-indices? g i j)
  (and
   (integer? i)
   (integer? j)
   (< -1 i (grid-w g))
   (< -1 j (grid-h g))))

(define (grid-index-at g i j)
  (+ (* j (grid-w g)) i))
(define (grid-set! g i j v)
  (vector-set! (grid-data g)
               (grid-index-at g i j)
               v))
(define (grid-ref g i j)
  (vector-ref (grid-data g)
              (grid-index-at g i j)))

(define (grid-dump g [elem->string ~a])
  (for ([j (in-range (grid-h g))])
    (for ([i (in-range (grid-w g))])
      (display (elem->string (grid-ref g i j))))
    (display #\newline)))
