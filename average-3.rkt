#lang racket
(define (average-3 lst);
  (define lst-3 (filter (lambda(x)(= 0 (remainder x 3))) lst))
  (define sum (foldl + 0 lst-3))
  (define len-lst-3 (length lst-3))
  (if (= len-lst-3 0)
      #f
      (/ sum len-lst-3)
      )
  )