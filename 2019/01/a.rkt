#lang racket

(define (fuel f)
  (- (quotient f 3) 2))

(apply + (map (compose1 fuel string->number) (sequence->list (in-lines))))
