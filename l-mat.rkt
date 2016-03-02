#lang racket
(define (L-mat? mat)
  (define len (length mat))
  (define (iter n tail)
    (cond
      [(null? tail) #t]
      [(= 1 (length (car tail))) #f]
      ((= (sub1 n)(count (λ(x)(equal? x 0))(list-tail (car tail)(- len (sub1 n))))) (iter (sub1 n) (cdr tail)))
      [else #f]
      ))
  (iter len mat))
