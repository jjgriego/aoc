#lang racket

(require "util/grid.rkt")

(define shapes-original
  (for/list ([line (in-lines (open-input-file "../inputs/14"))])
    (for/list ([pair-str (string-split line " -> ")])
      (match-define (list x-str y-str) (string-split pair-str ","))
      (cons (string->number x-str)
            (string->number y-str)))))

(define-values (board-min-x board-max-x board-min-y board-max-y)
  (for*/fold ([x-min 1000]
              [x-max 0]
              [y-min 1000]
              [y-max 0])
            ([shape shapes-original]
             [coord shape])
    (values
     (min x-min (car coord))
     (max x-max (car coord))
     (min y-min (cdr coord))
     (max y-max (cdr coord)))))

(define board-origin-x (- board-min-x 3))
(define board-origin-y 0)
(define board-extent-x (+ 6 (- board-max-x board-origin-x)))
(define board-extent-y (+ 3 (- board-max-y board-origin-y)))

(define shapes
  (for/list ([shape shapes-original])
    (for/list ([coord shape])
      (cons (- (car coord) board-origin-x)
            (- (cdr coord) board-origin-y)))))

(define board (make-grid board-extent-x board-extent-y #f))

(define sand-source-x (- 500 board-origin-x))
(define sand-source-y (- 0 board-origin-y))

(for ([shape shapes])
  (unless (> (length shape) 1) (error 'shape-too-short-?))
  (for ([start shape]
        [end (cdr shape)])
    (unless (or (= (car start) (car start))
                (= (cdr start) (cdr start)))
      (error 'not-axis-aligned))
    (for* ([x (in-inclusive-range (min (car start) (car end))
                                  (max (car start) (car end)))]
           [y (in-inclusive-range (min (cdr start) (cdr end))
                                  (max (cdr start) (cdr end)))])
      (grid-set! board x y 'wall))))

(define-values (left-edge-min-y right-edge-min-y) (values board-extent-y board-extent-y))

(define (add-sand
         [x sand-source-x]
         [y sand-source-y]
         #:floor? [has-floor? #f])
  (define (add-sand* x y)
    (add-sand x y #:floor? has-floor?))
  (cond
    ;; on the floor?
    [(and has-floor? (> y board-max-y))
     (grid-set! board x y 'sand)
     #t]
    ;; off the bottom?
    [(and (not has-floor?)
          (> y board-max-y))
     #f]
    ;; cell below is not occupied?
    [(not (grid-ref board x (add1 y)))
     (add-sand* x (add1 y))]
    ;; if we are on the left edge, assume there is no path to the left,
    ;; otherwise, check the cell below and to the left is not occupied?
    [(and (> x 0)
          (not (grid-ref board (sub1 x) (add1 y))))
     (add-sand* (sub1 x) (add1 y))]
    ;; try the right too
    [(and (< x (sub1 board-extent-x))
          (not (grid-ref board (add1 x) (add1 y))))
     (add-sand* (add1 x) (add1 y))]
    ;; otherwise stuck
    ;; if on edges, record the highest level needed
    [else
     (cond
       [(= x 0) (set! left-edge-min-y (min left-edge-min-y y))]
       [(= x (sub1 board-extent-x)) (set! right-edge-min-y (min right-edge-min-y y))])
     (grid-set! board x y 'sand)
     #t]))

(define (show-board)
  (grid-dump board (match-lambda
                     [#f "  "]
                     ['wall "##"]
                     ['sand "()"])))

(begin
  (define grain-count
    (let loop ([grains 0])
      (cond
        [(add-sand) (loop (add1 grains))]
        [else grains])))
  #;(show-board)
  grain-count)

(define count
  (let loop ([grains 0])
    (add-sand #:floor? #t)
    (cond
      [(grid-ref board sand-source-x sand-source-y)
       (add1 grains)]
      [else (loop (add1 grains))])))

#;(show-board)

(define (triangle n)
  (/ (* n (sub1 n)) 2))

(define (height-to-floor y)
  (+ 2 (- board-max-y y)))

(+ count
   (triangle (height-to-floor left-edge-min-y))
   (triangle (height-to-floor right-edge-min-y)))
