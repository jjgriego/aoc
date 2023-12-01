#lang racket

(define (rps-< a b)
  (match* (a b)
    [('paper 'rock) #t]
    [('rock 'scissors) #t]
    [('scissors 'paper) #t]
    [(_ _) #f]))

(define (opponent-char->rps c)
  (match c
    [#\A 'rock]
    [#\B 'paper]
    [#\C 'scissors]
    [else (error 'opponent-char->rps)]))

(define (player-char->rps c)
  (match c
    [#\X 'rock]
    [#\Y 'paper]
    [#\Z 'scissors]
    [else (error 'player-char->rps)]))

(define (player-char->outcome c)
  (match c
    [#\X 'loss]
    [#\Y 'draw]
    [#\Z 'win]))

(define (find-rps-outcome opp outcome)
  (match outcome
    ['draw opp]
    ['win
     (match opp
       ['rock 'paper]
       ['paper 'scissors]
       ['scissors 'rock])]
    ['loss
     (match opp
       ['rock 'scissors]
       ['paper 'rock]
       ['scissors 'paper])]))


(define (rps-base-score r)
  (match r
    ['rock 1]
    ['paper 2]
    ['scissors 3]))

#;(define input
  (for/list ([line (in-lines (open-input-file "../inputs/02"))])
    (match (string->list line)
      [(list a #\space b)

       (define opp (opponent-char->rps a))
       (define pla (player-char->rps b))

       (define result-score (cond
                              [(rps-< opp pla)
                               0]
                              [(equal? opp pla)
                               3]
                              [(rps-< pla opp)
                               6]))

       (+ (rps-base-score pla) result-score)])))


(define input
    (for/list ([line (in-lines (open-input-file "../inputs/02"))])
      (match (string->list line)
        [(list a #\space b)

         (define opp (opponent-char->rps a))
         (define outcome (player-char->outcome b))
         (define pla (find-rps-outcome opp outcome))

         (define result-score (match outcome ['loss 0] ['draw 3] ['win 6]))

         (+ (rps-base-score pla) result-score)])))

 
