#lang racket

(define (fuel x)
  (- (quotient x 3) 2))

(define (r-fuel x)
  (let ((result (fuel x)))
    (if (result . > . 0)
        (+ (r-fuel result) result)
        0)))

(apply + (map (compose1 r-fuel string->number) (sequence->list (in-lines))))
