#lang racket

(define points
  (for/list ([line (in-lines (open-input-file "../inputs/18"))])
    (map string->number (string-split line ","))))

(define points-set (list->set points))

(define directions (list
                    (list 1 0 0)
                    (list -1 0 0)
                    (list 0 1 0)
                    (list 0 -1 0)
                    (list 0 0 1)
                    (list 0 0 -1)))

(time
 (for*/sum ([point points]
            [dv directions])
   (define point* (map + point dv))
   (cond
     [(set-member? points-set point*) 0]
     [else 1])))


(define xmin (apply min (map first points)))
(define xmax (apply max (map first points)))
(define ymin (apply min (map second points)))
(define ymax (apply max (map second points)))
(define zmin (apply min (map third points)))
(define zmax (apply max (map third points)))

(define cache (make-hash))

(define (exterior? point)
  (cond
    [(not
      (and
       (<= xmin (first point) xmax)
       (<= ymin (second point) ymax)
       (<= zmin (third point) zmax)))
     (delay/strict #t)]
    [else
     (hash-ref! cache
                point
                (lambda ()
                  (delay
                    (cond
                      [(set-member? points-set point) #f]
                      [else
                       (define promises (for/list ([dv directions])
                                          (exterior? (map + point dv))))

                       (or
                        ;; are any immediately available and #t ?
                        (for/or ([p promises])
                          (and (promise-forced? p)
                               (force p)))
                        ;; otherwise try all runnable ones until we find a #t
                        (for/or ([p promises])
                          (and (not (promise-running? p))
                               (force p))))]))))]))


(time
 (for*/sum ([point points]
            [dv directions])
   (define point* (map + point dv))
   (cond
     [(not (force (exterior? point*))) 0]
     [else 1])))
