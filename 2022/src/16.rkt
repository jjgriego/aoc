#lang racket

(define input
  (for/list ([line (in-lines (open-input-file "../inputs/16"))])
    (match-define (list _ name flow dests)
      (regexp-match
       #rx"Valve ([A-Z]+) has flow rate=([0-9]+); tunnels? leads? to valves? ([A-Z ,]+)"
       line))

    (list (string->symbol name)
          (string->number flow)
          (map string->symbol
               (string-split dests
                             #rx"[, ]+")))))

(define meaningful-nodes
  (map first (filter (match-lambda [(list _ flow _) (> flow 0)])
                     input)))

(define shortest-paths
  (time
   (for/hash ([dest meaningful-nodes])
     (values
      dest
      (let ([paths (make-hash)]
            [reached (mutable-set)])
        (let loop ([frontier (map (curry list (list dest) dest) (third (assoc dest input)))])
          (match frontier
            ['() paths]
            [(cons (list path from to) more)
             (cond
               [(set-member? reached to)
                (loop more)]
               [else
                (set-add! reached to)
                (hash-set! paths to path)
                (loop (append more (map (curry list (cons to path) to)
                                        (third (assoc to input)))))])])))))))

(displayln meaningful-nodes)
(displayln (* (expt 2 (length meaningful-nodes))
              30
              (length input)
              (length input)))

(define (search node remaining gas)
  (apply values
         (argmax
          first
          (cons (list 0 remaining)
                (filter identity
                        (for/list ([target remaining])
                          (define path (hash-ref (hash-ref shortest-paths target) node))
                          (match-define (list _ flow _) (assoc target input))
                          (and (> gas (length path))
                               (let-values ([(v remaining) (search target (remove target remaining) (- gas (length path) 1))])
                                 (list
                                  (+ v (* (- gas (length path) 1) flow))
                                  remaining)))))))))

(time (search 'AA meaningful-nodes 30))

;; i can't justify this greedy solution except via intuition but it does work
(define-values (partial remaining) (time (search 'AA meaningful-nodes 26)))
(define-values (elephant-partial _) (time (search 'AA remaining 26)))
(+ partial elephant-partial)



