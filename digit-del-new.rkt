#lang racket
(define (del n)
  (define (nch x)
    (if (= x 0)
        #t
        (let ((y (remainder x 10)))
          (if (or (= y 0)(> (remainder x y)0))
              #t
              (nch (quotient x 10))
              )
          )
     )
    )
  (nch n)
  )
