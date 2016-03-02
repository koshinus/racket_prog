#lang racket
(define (cross a b c d)
  (define x (- (min b d)(max a c)))
  (if (> x 0)
      x
      0)
  )