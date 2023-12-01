#lang racket

(define resource-count 4)

(struct blueprint
  (ore-bot-cost clay-bot-cost obsidian-bot-cost geode-bot-cost)
  #:transparent)

(define input
  (for/list ([line (in-lines (open-input-file "../examples/19"))])
    (match-define (list ore-ore clay-ore
                        obs-ore obs-clay
                        geode-ore geode-obs)
      (map string->number
           (cdr
            (string-split line #rx"[^0-9]+"))))
    (vector
     (vector ore-ore 0 0 0)
     (vector clay-ore 0 0 0)
     (vector obs-ore obs-clay 0 0)
     (vector geode-ore 0 geode-obs 0))))

(define (basket-<? b1 b2)
  (for/and ([c1 b1]
            [c2 b2])
    (< c1 c2)))
(define (basket-<=? b1 b2)
  (for/and ([c1 b1]
            [c2 b2])
    (<= c1 c2)))

(define (basket-add b1 b2)
  (vector-map + b1 b2))
(define (basket-sub b1 b2)
  (vector-map + b1 b2))
(define (basket-mul c b)
  (vector-map (curry * c) b))
(define (basket-quotient n m)
  (apply min (for/list ([x n]
                        [y m])
               (quotient x y))))

(define (basket-can-cover? n m)
  (for/and ([x n] [y m])
    (or (<= x 0)
        (> y 0))))

(define (basket-cover n m)
  (apply max (for/list ([x n]
                        [y m])
               (cond
                 [(<= x 0) 0]
                 [else
                  (quotient (+ x (sub1 y)) y)]))))

(define (basket-copy b)
  (vector-copy b))

(define (basket-inc idx b)
  (define b* (vector-copy b))
  (vector-set! b* idx (add1 (vector-ref b* idx)))
  b*)

(define (basket-geodes b)
  (vector-ref b 3))

(define indent (make-parameter ""))

(define (call-with-indent th)
  (parameterize ([indent (string-append "  " (indent))])
    (th)))

(define cache (make-hash))

#;(define (optim-geodes ttl resources bots recipes)
  ;; choose the best bot to build this cycle
  ;; (which includes the wait time to accrue the necessary resources)

  (display (indent))
  (displayln (list ttl resources bots))

  (hash-ref!
   cache
   (list ttl resources bots)
   (lambda ()
     (apply
      max
      (filter
       identity
       (for/list ([r (in-range resource-count)])
         (define cost (vector-ref recipes r))
         (cond
           [(< ttl 1)
            (basket-geodes resources)]
           ;; bot is immediately constructable?
           [(basket-<=? cost resources)
            #;(display (indent))
            #;(displayln (list 'immediate r))
            (define ttl* (sub1 ttl))
            (define resources* (basket-add (basket-sub resources cost)
                                           bots))
            (define bots* (basket-inc r bots))
            (call-with-indent
             (lambda ()
               (optim-geodes ttl* resources* bots* recipes)))]
           [(not (basket-can-cover? (basket-sub cost resources)
                                    bots))
            ;; waiting will never allow us to build this bot
            #f]
           [else
            (define wait-cycles (basket-cover (basket-sub cost resources)
                                              bots))
            #;(display (indent))
            #;(displayln (list 'wait r wait-cycles))
            (cond
              [(< wait-cycles ttl)
               (define ttl* (- ttl wait-cycles))
               (define bots* (basket-inc r bots))
               (define resources* (basket-add (basket-sub resources cost)
                                              (basket-mul wait-cycles bots)))
               (call-with-indent
                (lambda ()
                  (optim-geodes ttl* resources* bots* recipes)))]
              [else
               ;; just run out the ttl
               (define resources* (basket-add resources (basket-mul ttl bots)))
               (basket-geodes resources*)])])))))))


