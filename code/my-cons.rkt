#lang racket

(define (my-cons fst snd)
  (lambda(pick)
    (cond [(= pick 1) fst]
          [(= pick 2) snd]
          [#t (error "error pick")])))

(define (my-car pair)
  (pair 1))

(define (my-cdr pair)
  (pair 2))
