#lang racket

(struct item-worry (moduli residues) #:transparent)

(define (integer->item-worry moduli x)
  (item-worry
   moduli
   (for/vector ([u moduli])
     (remainder x u))))

(define (item-worry+ w1 w2)
  (item-worry
   (item-worry-moduli w1)
   (for/vector ([u (item-worry-moduli w1)]
                [r1 (item-worry-residues w1)]
                [r2 (item-worry-residues w2)])
     (remainder (+ r1 r2) u))))

(define (item-worry* w1 w2)
  (item-worry
   (item-worry-moduli w1)
   (for/vector ([u (item-worry-moduli w1)]
                [r1 (item-worry-residues w1)]
                [r2 (item-worry-residues w2)])
     (remainder (* r1 r2) u))))

(define (item-worry-divides? w d)
  (define idx (for/or ([idx (in-naturals)]
                       [u (item-worry-moduli w)])
                (and (= u d) idx)))
  (unless idx (error 'item-worry-divides?))
  (= 0 (vector-ref (item-worry-residues w) idx)))

(struct monkey (items inspector modulus score) #:transparent #:mutable)

(define (port->monkeys [in (current-input-port)])
  ;; ignore monkey index
  (cond
    [(eof-object? (read-line in)) empty-stream]
    [else
     ;; read items
     (define items (map string->number (string-split (string-trim (read-line in) #rx" *Starting items: ") ", ")))

     ;; read operation
     (define (read-expr)
       (match (regexp-match #rx"(old|[0-9]*)" in)
         [(cons #"old" _)
          identity]
         [(cons n-str _)
          (let ([n (string->number (bytes->string/utf-8 n-str))])
            (lambda (x)
              (integer->item-worry (item-worry-moduli x) n)))]))

     (define (read-op)
       (match (regexp-match #rx" *([*+]) *" in)
         [(list _ #"+") item-worry+]
         [(list _ #"*") item-worry*]))

     (regexp-match " *Operation: new = " in)
     (define lhs (read-expr))
     (define op (read-op))
     (define rhs (read-expr))
     (read-line in)


     (define modulus (string->number (string-trim (read-line in) #rx" *Test: divisible by ")))
     (define true-dest (string->number (string-trim (read-line in) #rx" *If true: throw to monkey ")))
     (define fals-dest (string->number (string-trim (read-line in) #rx" *If false: throw to monkey ")))

     (define (operation x)
       #;(define x* (quotient (op (lhs x) (rhs x))
                            3))
       (define x* (op (lhs x) (rhs x)))
       (values
        x*
        (if (item-worry-divides? x* modulus)
            true-dest
            fals-dest)))
     (read-line in)
     (stream-cons
      (monkey items operation modulus 0)
      (port->monkeys in))]))

(define monkeys (list->vector (stream->list (port->monkeys (open-input-file "../inputs/11")))))

(define moduli (vector-map monkey-modulus monkeys))

(for ([m monkeys])
  (set-monkey-items! m (map (curry integer->item-worry moduli)
                            (monkey-items m))))

(define (monkey-receive m item)
  (set-monkey-items! m (append (monkey-items m) (list item))))

(define (monkey-inspect i)
  (define m (vector-ref monkeys i))
  (define items (monkey-items m))
  (set-monkey-items! m '())
  (for ([item items])
    (define-values (item* dest) ((monkey-inspector m) item))
    (set-monkey-score! m (add1 (monkey-score m)))
    (when (= dest i) (error "should not happen?"))
    (monkey-receive (vector-ref monkeys dest) item*)))

(time
 (for ([_ (in-range 10000)])
   #;(pretty-print monkeys)
   (for ([i (in-range (vector-length monkeys))])
     (monkey-inspect i)
     )))


(apply * (take (sort (vector->list (vector-map monkey-score monkeys )) >)
      2))
