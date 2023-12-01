#lang racket

(provide
 heap?
 heap-size
 heap-elem-<?
 make-heap
 heap-add!
 heap-remove!)

(struct heap (size contents elem-<?) #:transparent #:mutable)

(define (heap-reserve-capacity! h n)
  (define old-size (vector-length (heap-contents h)))
  (when (< old-size n)
    ;; growth strategy: just double the size
    (define new-size (* 2 old-size))
    (define new-contents (make-vector new-size))
    (vector-copy! new-contents 0 (heap-contents h))
    (set-heap-contents! h new-contents)))

(define (make-heap [elem-<? <])
  (heap 0 (make-vector 16) elem-<?))

(define (idx-left i)
  (* 2 i))
(define (idx-right i)
  (add1 (* 2 i)))

(define (heap-ref h idx)
  (vector-ref (heap-contents h) (sub1 idx)))
(define (heap-set! h idx x)
  (vector-set! (heap-contents h) (sub1 idx) x))
(define (heap-swap! h idx x)
  (define old (heap-ref h idx))
  (heap-set! h idx x)
  old)

(define (heap-add! h x)
  (define elem-<? (heap-elem-<? h))
  (define old-size (heap-size h))
  (define new-size (add1 old-size))
  (heap-reserve-capacity! h new-size)
  (set-heap-size! h new-size)
  (heap-set! h new-size x)
  (unless (= 0 old-size)
    (heapify-up h new-size x elem-<?))
  ;;              ^--- heap indices are shifted by one
  (void))

(define (heapify-up h idx x elem-<?)
  (define parent (arithmetic-shift idx -1))
  (define parent-x (heap-ref h parent))
  (when (elem-<? x parent-x)
    (heap-set! h idx parent-x)
    (heap-set! h parent x)
    (unless (<= parent 1)
      (heapify-up h parent x elem-<?))))

(define (heap-remove! h)
  (cond
    [(> (heap-size h) 0)
     (define old-size (heap-size h))
     (set-heap-size! h (sub1 old-size))
     (define x (heap-ref h old-size))
     (begin0 (heap-ref h 1)
       (heapify-down h 1 x (heap-elem-<? h)))]
    [else
     #f]))

(define (heapify-down h idx x elem-<?)
  (define il (idx-left idx))
  (define ir (idx-right idx))
  (define n (heap-size h))

  (define (continue-with i)
    (define y (heap-ref h i))
    (heap-set! h idx y)
    (heapify-down h i x elem-<?))

  (cond
    ;; both children are outside the heap
    [(> il n) (heap-set! h idx x)]
    ;; the right child is outside the heap
    [(> ir n)
     (define xl (heap-ref h il))
     (cond
       [(elem-<? x xl) (heap-set! h idx x)]
       [else (continue-with il)])]
    [else
     (define xl (heap-ref h il))
     (define xr (heap-ref h ir))
     (cond
       [(and (elem-<? x xl)
             (elem-<? x xr))
        (heap-set! h idx x)]
       [(elem-<? xl xr)
        (continue-with il)]
       [else
        (continue-with ir)])]))

