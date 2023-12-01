#lang racket

(struct vm (cycle-count pending-interrupts pc code register) #:transparent #:mutable)

(define (make-vm code interrupt-queue)
  (vm 1 interrupt-queue 0 code 1))

(define (vm-halted? vm)
  (>= (vm-pc vm)
      (length (vm-code vm))))

(define (vm-interact vm)
  (let loop ([irqs (vm-pending-interrupts vm)])
    (cond
      [(and (not (empty? irqs))
            (>= (vm-cycle-count vm) (caar irqs)))
       ((cdar irqs) vm)
       (loop (cdr irqs))]
      [else
       (set-vm-pending-interrupts! vm irqs)]))

  (define x-pos (remainder (sub1 (vm-cycle-count vm))
                           40))
  (define pixel (if (<= (sub1 x-pos) (vm-register vm) (add1 x-pos))
                   "##" ".."))
  (display pixel)
  (when (= x-pos 39)
    (display #\newline)))

(define (interp-one vm)
  (vm-interact vm)
  (define-values (cycles reg)
    (match (list-ref (vm-code vm) (vm-pc vm))
      [(list 'noop)
       (values 1 (vm-register vm))]
      [(list 'addx n)
       (values 2 (+ n (vm-register vm)))]))

  (for ([_ (in-range (sub1 cycles))])
    (set-vm-cycle-count! vm (add1 (vm-cycle-count vm)))
    (vm-interact vm))

  (set-vm-cycle-count! vm (add1 (vm-cycle-count vm)))
  (set-vm-register! vm reg)
  (set-vm-pc! vm (add1 (vm-pc vm))))

(define (vm-run vm)
  (let loop ()
    (unless (vm-halted? vm)
      (interp-one vm)
      (loop))))

(define (read-code [in (open-input-file "../inputs/10")])
  (for/list ([line (in-lines in)])
    (match (string-split line)
      [(list "noop") '(noop)]
      [(list "addx" n-str)
       (list 'addx (string->number n-str))])))

(define input-code (read-code))

(define result 0)
(define (record-landmark-signal-strength cycle vm)
  (set! result (+ result (* cycle (vm-register vm)))))

(vm-run (make-vm input-code
         (for/list ([cycle (list 20 60 100 140 180 220)])
           (cons cycle (curry record-landmark-signal-strength cycle)))))

(displayln result)
