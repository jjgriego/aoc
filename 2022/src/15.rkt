#lang racket

(module+ test
  (require rackunit))

(define (in-interval? b e x)
  (and (<= b x) (< x e)))

(define (interval-measure b e)
  (- e b))

(define (interval-set-measure is)
  (for/sum ([i is])
    (interval-measure (car i) (cdr i))))

(define (interval-set-contains? is x)
  (match is
    [(list) #f]
    [(cons (cons b e) more)
     (or (in-interval? b e x)
         (interval-set-contains? more))]))

(define (interval-set-merge-one is start end)
  (match is
    [(list) (list (cons start end))]

    [(list* (cons b e) more)
     #:when (and (in-interval? b (add1 e) start)
                 (in-interval? b e end))
     is]

    [(list* (cons b e) more)
     #:when (in-interval? b e end)
     (list* (cons start e) more)]

    [(list* (cons b e) more)
     #:when (in-interval? b (add1 e) start)
     (interval-set-merge-one more b end)]

    [(list* (cons b e) more)
     #:when (and (in-interval? start (add1 end) b)
                 (in-interval? start end e))
     (interval-set-merge-one more start end)]

    [(list* (cons b e) more)
     #:when (> start b)
     (cons (cons b e) (interval-set-merge-one more start end))]

    [(list* (cons b e) more)
     (list* (cons start end) (cons b e) more)]))

(module+ test
  (check-equal? (interval-set-merge-one '((0 . 5) (7 . 10))
                                        5 7)
                '((0 . 10)))
  (check-equal? (interval-set-merge-one '((0 . 5) (7 . 10))
                                        0 7)
                '((0 . 10)))
  (check-equal? (interval-set-merge-one '((0 . 5) (7 . 10))
                                        9 12)
                '((0 . 5) (7 . 12)))
  (check-equal? (interval-set-merge-one '((0 . 5) (7 . 10))
                                        -5 100)
                '((-5 . 100)))
  (check-equal? (interval-set-merge-one '((0 . 5) (7 . 10))
                                        -100 -1)
                '((-100 . -1) (0 . 5) (7 . 10)))
  (check-equal? (interval-set-merge-one '((0 . 5) (7 . 10))
                                        100 101)
                '((0 . 5) (7 . 10) (100 . 101))))

(define (interval-set-merge xs ys)
  (foldl (lambda (ival r)
           (interval-set-merge-one r (car ival) (cdr ival)))
         xs
         ys))

(module+ test
  (check-equal? (interval-set-merge '((0 . 2) (4 . 6))
                                    '((2 . 4)))
                '((0 . 6)))
  (check-equal? (interval-set-merge '() '((1 . 10) (12 . 35)))
                '((1 . 10) (12 . 35))))


(define (list*/discarding-empty-intervals . xs)
  (match xs
    [(list more) more]
    [(cons (cons b e) more)
     #:when (= b e)
     (apply list*/discarding-empty-intervals more)]
    [(cons (cons b e) more)
     (cons (cons b e) (apply list*/discarding-empty-intervals more))]))

(define (interval-set-remove-one is start end)
  (match is
    [(list) (list)]
    ;; [----------)     [-------)
    ;; [-----------)
    [(list* (cons b e) more)
     #:when (and (in-interval? start end b)
                 (in-interval? start (add1 end) e))
     (interval-set-remove-one more start end)]

    ;; [----------)     [-------)
    ;;  [---------)
    [(list* (cons b e) more)
     #:when (and (in-interval? b e start)
                 (in-interval? b (add1 e) end))
     (list*/discarding-empty-intervals (cons b start) (cons end e)
                                       more)]

    ;; [----------)     [-------)
    ;;  [------------...)
    [(list* (cons b e) more)
     #:when (in-interval? b e start)
     (list*/discarding-empty-intervals (cons b start)
                                       (interval-set-remove-one more start end))]

    ;;       [----------)     [-------)
    ;;  [...------------)
    [(list* (cons b e) more)
     #:when (in-interval? b (add1 e) end)
     (list*/discarding-empty-intervals (cons end e)
                                       more)]

    ;; we know the intervals are disjoint now
    ;;         [----------)     [-------)
    ;;  [...)
    [(list* (cons b e) more)
     #:when (<= end b)
     is]

    ;; we know the intervals are disjoint now
    ;;  [----------)     [-------)
    ;;               [...---)
    [(list* (cons b e) more)
     (cons (cons b e)
           (interval-set-remove-one more start end))]))

(define (interval-set-subtract xs ys)
  (foldl (lambda (ival r)
           (interval-set-remove-one r (car ival) (cdr ival)))
         xs
         ys))

(module+ test
  (check-equal? (interval-set-subtract '((0 . 100))
                                       '((5 . 15)))
                '((0 . 5) (15 . 100)))
  (check-equal? (interval-set-subtract '((0 . 100))
                                       '((-5 . 15)))
                '((15 . 100)))
  (check-equal? (interval-set-subtract '((0 . 100))
                                       '((-5 . -2)))
                '((0 . 100)))
  (check-equal? (interval-set-subtract '((0 . 100))
                                       '((100 . 101)))
                '((0 . 100)))
  (check-equal? (interval-set-subtract '((0 . 100))
                                       '((99 . 101)))
                '((0 . 99)))
  (check-equal? (interval-set-subtract '((0 . 100))
                                       '((50 . 100)))
                '((0 . 50)))
  (check-equal? (interval-set-subtract '((0 . 100))
                                       '((0 . 100)))
                '())
  (check-equal? (interval-set-subtract '((0 . 100) (150 . 200))
                                       '((99 . 201)))
                '((0 . 99))))

(define input
  (for/list ([line (in-lines (open-input-file "../inputs/15"))])
    (match-define (list _ sx sy bx by)
      (regexp-match #rx"Sensor at x=([-0-9]*), y=([-0-9]*): closest beacon is at x=([-0-9]*), y=([-0-9]*)"
                    line))
    (map string->number (list sx sy bx by))))

(define (excluded-beacon-x-in-row row-of-interest)
  (foldl interval-set-merge
         '()
         (for/list ([sensor-record input])
           (match-define (list sx sy bx by) sensor-record)
           (define distance (+ (abs (- sx bx))
                               (abs (- sy by))))
           ;; | px - sx | + | row-of-interest - sy | <= distance
           ;; | px - sx | <= distance - | row-of-interest - sy |
           (define y-distance-to-row (abs (- sy row-of-interest)))
           (define x-distance-threshold (- distance y-distance-to-row))
           (cond
             [(< x-distance-threshold 0)
              ;; too far to count
              (list)]
             [else
              (list (cons
                     (- sx x-distance-threshold)
                     (add1 (+ sx x-distance-threshold))))]))))

(let* ([row-of-interest 2000000]
       [intervals (excluded-beacon-x-in-row row-of-interest)])
  (- (interval-set-measure intervals)
     (for/sum ([beacon (list->set (map
                                   (match-lambda
                                     [(list _ _ bx by)
                                      (cons bx by)])
                                   input))])
       (match-define (cons bx by) beacon)
       (cond
         [(and (= by row-of-interest)
               (interval-set-contains? intervals bx))
          1]
         [else 0]))))

(match-define (list y (list (cons x _)))
  (for/or ([y (in-inclusive-range 0 4000000)])
    (define ivals (interval-set-subtract
                   '((0 . 4000001))
                   (excluded-beacon-x-in-row y)))
    (and (= 1 (interval-set-measure ivals))
         (list y ivals))))

(+ y (* x 4000000))











