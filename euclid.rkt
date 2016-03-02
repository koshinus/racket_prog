#lang racket
(define (euclid x y)
  (if (= y 0)
      x
      (euclid y (remainder x y))))