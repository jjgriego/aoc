#lang racket

(module+ test
  (require rackunit))

(require racket/stream)
(require "util/grid.rkt")

(define shapes
  (let ([_ 1])
    (map (curry grid-map zero?)
         (list
          (grid 4 1 (vector 0 0 0 0))
          (grid 3 3 (vector _ 0 _
                            0 0 0
                            _ 0 _))
          (grid 3 3 (vector 0 0 0
                            _ _ 0
                            _ _ 0))
          (grid 1 4 (vector 0
                            0
                            0
                            0))
          (grid 2 2 (vector 0 0
                            0 0))))))

(define (in-shape-coords shape)
  (for/stream ([coord (in-grid-coordinates shape)]
               #:when (grid-ref shape
                                (car coord)
                                (cdr coord)))
    coord))

(struct stage (y-base row-base contents) #:transparent)

(define (empty-stage)
  (stage
   0
   0
   (make-grid 7 10 #f)))

(define (stage-contents-copy-canonical! src src-row-base dst)
  ;; copy the back half of the circular buffer
  (define back-size (- (grid-h src)
                       src-row-base))
  (grid-copy! src
              dst
              0 src-row-base
              0 0
              7 back-size)
  ;; now the front half
  (grid-copy! src
              dst
              0 0
              0 back-size
              7 src-row-base))

(define (stage-copy st)
  (stage
   (stage-y-base st)
   (stage-row-base st)
   (grid-copy (stage-contents st))))

(define (stage-equiv-class st)
  ;; only the canonical contents are relevant for the hash algo
  (define contents (make-grid (grid-w (stage-contents st))
                              (grid-h (stage-contents st))
                              #f))
  (stage-contents-copy-canonical! (stage-contents st) (stage-row-base st)
                                  contents)
  contents)

(define (stage-buf-coords st x y)
  (values
   x
   (remainder (+ (- y (stage-y-base st))
                 (stage-row-base st))
              (grid-h (stage-contents st)))))


(define (stage-ref st x y)
  (cond
    [(< y (stage-y-base st))
     #t]
    [(>= y (stage-y-capacity st))
     (error 'stage-ref)]
    [else
     (define-values (i j) (stage-buf-coords st x y))
     (grid-ref (stage-contents st) i j)]))

(define (stage-set! st x y v)
  (when (< y (stage-y-base st))
    (error 'scream))
  (define-values (i j) (stage-buf-coords st x y))
  (grid-set! (stage-contents st) i j v))

(define (stage-y-capacity st)
  (+ (stage-y-base st) (grid-h (stage-contents st))))

(define max-stage-height (* 1024 1024))
(define (stage-reserve-height st ymax)
  (cond
    [(< ymax (stage-y-capacity st)) st]
    [else
     ;; try compaction first
     (define-values (st* recovered) (stage-compact st))

     (displayln (list 'compact recovered))

     ;; if headroom is not at least half the buffer, grow
     (define minimum-headroom (/ (grid-h (stage-contents st*))
                                 2))

     (cond
       [(>= recovered minimum-headroom)
        st*]
       [else
        ;; growth strategy: double
        (define contents* (make-grid 7 (* 2 (grid-h (stage-contents st*))) #f))

        (displayln (list 'grow (grid-h contents*)))

        (stage-contents-copy-canonical! (stage-contents st*)
                                        (stage-row-base st*)
                                        contents*)

        (stage (stage-y-base st*)
               0
               contents*)])]))

(module+ test
  (check-equal? (stage-reserve-height
                 (stage 0 0
                        (grid 7 7
                              (vector
                               #t #f #f #f #f #f #f ; 0
                               #t #f #f #f #f #f #f ; 1
                               #t #f #f #f #f #f #f ; 2
                               #t #f #f #f #f #f #f ; 3
                               #t #f #f #f #f #f #f ; 4
                               #t #t #t #t #t #t #t ; 5
                               #f #f #f #f #f #f #f
                               )
                              ))
                 9)
                (stage
                 6 6
                 (grid 7 7
                       (vector
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        ))))

  #;(check-equal? (stage-reserve-height
                 (stage
                  6 6
                  (grid 7 7
                        (vector
                         #t #f #f #f #f #f #f ; 0
                         #t #f #f #f #f #f #f ; 1
                         #t #f #f #f #f #f #f ; 2
                         #t #f #f #f #f #f #f ; 3
                         #t #f #f #f #f #f #f ; 4
                         #t #t #t #t #t #t #t ; 5
                         #f #f #f #f #f #f #f
                         #f #f #f #f #f #f #f
                         )))
                 13)
                (stage
                 6 0
                 (grid 7 14
                       (vector
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f
                        )))


                )

  )




(define (stage-compact-y-base st)
  (define-values (x0 y0) (stage-start st))
  ;; we have to find the lowest y value that a shape can end up at
  ;; which effectively means we run a simulation with all possible shapes
  ;; and all possible jet choices
  (apply min
         (for/list ([shape shapes])
           (let loop ([x~ (list x0)]
                      [y y0])
             (define x~*
               (remove-duplicates
                (filter identity
                        (for*/list ([x x~]
                                    [dx (list -1 1)])
                          (define x* (cond
                                       [(stage-shape-fits? st shape (+ x dx) y)
                                        (+ x dx)]
                                       [else x]))
                          (and (stage-shape-fits? st shape x* (sub1 y))
                               x*)))))
             (cond
               [(empty? x~*) y] ; y is the lowest this shape can reach
               [(equal? (stage-y-base st) y) y]
               [else (loop x~* (sub1 y))])))))

(module+ test
  (check-equal? (stage-compact-y-base
                 (stage 0 0
                        (grid 7 7
                              (vector
                               #t #f #f #f #f #f #f ; 3
                               #t #f #f #f #f #f #f ; 4
                               #t #t #t #t #t #t #t ; 5
                               #f #f #f #f #f #f #f
                               #f #f #f #f #f #f #f
                               #f #f #f #f #f #f #f
                               #f #f #f #f #f #f #f
                               )
                              )))
                3)
  )

(define (stage-compact st)
  (define new-base (stage-compact-y-base st))
  (define old-base (stage-y-base st))
  (cond
    [(= old-base new-base) (values st 0)]
    [else
     (define discarded-rows (- new-base old-base))

     ;; zero out the rows we are discarding
     (for* ([dy (in-range discarded-rows)]
            [x (in-range 7)])
       (stage-set! st x (+ old-base dy) #f))

     (values
      (stage new-base
             (remainder
              (+ (stage-row-base st)
                 discarded-rows)
              (grid-h (stage-contents st)))
             (stage-contents st))
      discarded-rows)]))

(define (stage-shape-fits? st shape x y)
  (for/and ([coord (in-shape-coords shape)])
    (let ([x* (+ x (car coord))]
          [y* (+ y (cdr coord))])
      (and
       ;; in bounds
       (< -1 x* 7)
       (< -1 y*)
       ;; not occupied
       (or
        (>= y* (stage-y-capacity st))
        (not (stage-ref st x* y*)))))))

(define (stage-height st)
  (or
   (for/or ([y (in-range (stage-y-base st)
                         (stage-y-capacity st))])
     (and
      (for/and ([x (in-range 7)])
        (not (stage-ref st x y)))
      y))
   #;(error 'hmm)
   #;(begin
     (dump-stage st)
     (error 'hmm)
     )
   (stage-y-capacity st)))

(define (stage-start st)
  (define height (stage-height st))
  (define x0 2)
  (define y0 (+ height 3))
  (values x0 y0))

(define (stage-add-shape st shape x1 y1)
  (define st*
    (stage-reserve-height st (+ y1 (grid-h shape))))

  (for ([c (in-shape-coords shape)])
    (define x (+ x1 (car c)))
    (define y (+ y1 (cdr c)))
    (stage-set! st* x y #t))

  st*)


(define (dump-stage st)
  (define y0 (for/or ([y (in-inclusive-range
                          (sub1 (stage-y-capacity st))
                          (stage-y-base st)
                          -1)])
               (and
                (for/or ([x 7])
                  (stage-ref st x y))
                y)))

  (when y0
    (for ([y (in-inclusive-range y0 (stage-y-base st) -1)])
      (for ([x 7])
        (display (if (stage-ref st x y)
                     "()" ". ")))
      (display #\newline)))

  
  (displayln (stage-y-base st)))

(define-values (x0 y0) (stage-start (empty-stage)))

(define jets
  (filter
   (match-lambda [(or #\< #\>) #t] [_ #f])
   (port->list read-char
               (open-input-file "../examples/17"))))

#;(define jet-stream (sequence->stream (in-cycle (in-string ">"))))

(define (cycle-stream entries)
  (for/stream ([(ent idx) (in-cycle (in-indexed entries))])
    (cons idx ent)))



(define jets-stream (cycle-stream jets))
(define shapes-stream (cycle-stream shapes))

;; consumes a full cycle of jets, does not check rock-count for
;; termination
(define (one-cycle stage shapes x y rock-count #:rock-limit [rock-limit #f])
  (for/fold ([stage stage]
             [shapes shapes]
             [x x] [y y]
             [rock-count rock-count])
            ([jet jets]
             #:final (and rock-limit
                          (>= rock-count rock-limit)))
    (define shape (cdr (stream-first shapes)))
    (define x*
      (let ([dx (match jet [#\> 1] [#\< -1])])
        (cond
          [(stage-shape-fits? stage shape (+ x dx) y)
           (+ x dx)]
          [else x])))
    (cond
      [(stage-shape-fits? stage shape x* (sub1 y))
       (values stage
               shapes
               x* (sub1 y)
               rock-count)]
      [else
       (define stage* (stage-add-shape stage shape x* y))
       (define-values (x0 y0) (stage-start stage*))
       (define shapes-rest (stream-rest shapes))

        #;(let ([shape (stream-first shapes-rest)]
              [x x0]
              [y y0])
          (let ([c (stage-add-shape (stage-copy stage*) shape x y)])
            (displayln (list x y))
            (dump-stage c)
            (displayln "##############")))

        (values stage*
                shapes-rest
                x0 y0
                (add1 rock-count))])))

(struct memo-key (stage-contents shape-idx x y-off) #:transparent)
(struct memo-entry (next-key d-rocks d-y-base) #:transparent)
(struct memo-last (key rocks height) #:transparent)

(struct cycle (d-rocks d-y-base) #:transparent)



(define (simulate target-rock-count)
  (define cache (make-hash))

  (define cycles-computed-count 0)

  (let loop ([;; a canonical stage grid
              contents (stage-contents (empty-stage))]
             [;; the shape index
              shape-idx 0]
             ;; coordinates of the shape, relative to y-base
             [x 2] [y 3]
             [y-base 0]
             [rock-count 0]
             [identified-cycle #f])
    (define mk (memo-key contents shape-idx x y))
    (match* ((hash-ref cache mk #f)
             identified-cycle)
      [(_ (cycle d-rocks d-y-base))
       (define cycles (quotient (- target-rock-count rock-count)
                                d-rocks))
       (loop contents shape-idx x y
             (+ y-base (* cycles d-y-base))
             (+ rock-count (* cycles d-rocks))
             'wrap-up)]

      [((and start (memo-entry
                   (memo-key
                    contents* shape-idx* x* y*)
                   d-rocks d-y-base))
        #f)
       ;; if we have this one available, it means there's certainly now
       ;; a cycle of memo entries, we have to find it and find its length
       (let cycle-loop ([e (hash-ref cache (memo-entry-next-key start))]
                        [cycle-d-rocks (memo-entry-d-rocks start)]
                        [cycle-d-y-base (memo-entry-d-y-base start)])
         (cond
           [(eq? e start)
            ;; a full cycle takes us to the current state again and
            ;; augments by the cycle-d-* values: find out how many times we can
            ;; do that:
            (unless (> cycle-d-rocks 0) (error 'whah))
            (loop contents shape-idx x y y-base rock-count
                  (cycle cycle-d-rocks cycle-d-y-base))]
           [else
            (cycle-loop (hash-ref cache (memo-entry-next-key e))
                        (+ cycle-d-rocks (memo-entry-d-rocks e))
                        (+ cycle-d-y-base (memo-entry-d-y-base e)))]))]

      [((and start (memo-entry
                    (memo-key
                     contents* shape-idx* x* y*)
                    d-rocks d-y-base))
        'wrap-up)
       #:when (<= (+ rock-count d-rocks) target-rock-count)
       (loop contents* shape-idx* x* y*
             (+ y-base d-y-base)
             (+ rock-count d-rocks)
             identified-cycle)]

      [(_ _)
       ;; otherwise we need to compute, build the state and memo entry
       (define st (stage y-base 0 (grid-copy contents)))
       (define-values (st* shapes x* y-abs* rock-count*)
         (one-cycle st (stream-tail shapes-stream shape-idx)
                    x (+ y-base y) rock-count
                    #:rock-limit target-rock-count))

       (set! cycles-computed-count (add1 cycles-computed-count))

       (cond
         ;; did we stop early??
         [(= rock-count* target-rock-count)
          (printf "  :: actual simulations done: ~a~n" cycles-computed-count)
          (stage-height st*)]
         ;; otherwise, proceed w/ memo table
         [else
          (define y* (- y-abs* (stage-y-base st*)))
          (define y-base* (stage-y-base st*))
          (define d-y-base (- y-base* y-base))
          (define d-rocks (- rock-count* rock-count))
          (define contents* (stage-equiv-class st*))
          (define shape-idx* (car (stream-first shapes)))
          (hash-set! cache mk (memo-entry
                               (memo-key
                                contents*
                                shape-idx*
                                x*
                                y*)
                               d-rocks
                               d-y-base))
          (loop contents* shape-idx* x* y* y-base* rock-count* identified-cycle)])])))

(time (simulate 2022))
(time (simulate 1000000000000))

