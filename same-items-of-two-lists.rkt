#lang racket
(define (same-items-of-two-lists list1 list2)
  (foldl (λ (x y)(if (member x list2)
                     (cons x y)
                     y)) '() list1))