#lang racket

(require "util/heap.rkt")
(require "util/grid.rkt")

(define lines (port->lines (open-input-file "../inputs/12")))
(define heights (make-grid (string-length (car lines))
                           (length lines)))

(define problem-start #f)
(define problem-end #f)

(for ([line lines]
      [j (in-naturals)])
  (for ([c line]
        [i (in-naturals)])
    (define c* (match c
                 [#\S
                  (set! problem-start (cons i j))
                  #\a]
                 [#\E
                  (set! problem-end (cons i j))
                  #\z]
                 [_ c]))

    (grid-set! heights i j (- (char->integer c*)
                              (char->integer #\a)))))

(define cardinal-directions
  (list (cons 0 1)
        (cons -1 0)
        (cons 0 -1)
        (cons 1 0)))

(struct frontier-elem (i j path-length) #:transparent)

(define (frontier-elem-<? e1 e2)
  (< (frontier-elem-path-length e1)
     (frontier-elem-path-length e2)))

(define (search-path-length
         start
         end?
         valid-step?)

  ;; thanks, dijkstra

  (define reached (make-grid (grid-w heights)
                             (grid-h heights)
                             #f))

  (define frontier (make-heap frontier-elem-<?))
  (heap-add! frontier (frontier-elem (car start)
                                     (cdr start)
                                     0))

  (define (touch i j path-length)
    (grid-set! reached i j #t)
    (define h (grid-ref heights i j))

    (for ([dx cardinal-directions])
      (define di (car dx))
      (define dj (cdr dx))
      (define dest-i (+ i di))
      (define dest-j (+ j dj))
      (when (and
             (grid-valid-indices? heights dest-i dest-j)
             (valid-step? i j dest-i dest-j))
        (heap-add! frontier
                   (frontier-elem dest-i dest-j
                                  (add1 path-length))))))

  (define (step)
    (match (heap-remove! frontier)
      [(frontier-elem i j path-length)
       (unless (grid-ref reached i j)
         (touch i j path-length))
       (cond
         [(end? i j)
          path-length]
         [else
          (step)])]
      [#f #f]))

  (step))

(search-path-length problem-start
                    (lambda (i j)
                      (and (= i (car problem-end))
                           (= j (cdr problem-end))))
                    (lambda (i0 j0 i j)
                      (define height0 (grid-ref heights i0 j0))
                      (define height (grid-ref heights i j))
                      (<= height (add1 height0))))


(search-path-length problem-end
                    (lambda (i j)
                      (= (grid-ref heights i j) 0))
                    (lambda (i0 j0 i j)
                      (define height0 (grid-ref heights i0 j0))
                      (define height (grid-ref heights i j))
                      (>= height (sub1 height0))))
