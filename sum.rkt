#lang racket
(define (sum x)
  (if (and (>= x 0)(<= x 9))
      x
      (+(remainder x 10)(sum (quotient x 10)))))