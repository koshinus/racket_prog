#lang racket
(define (divs num)
  (define (inf s x d)
    (if (> (* d d) x)
        s
        (if (= (remainder x d) 0)
            (inf (cons d s)(/ x d))
            (inf s x (add1 d))
            )
        )
    )
  (inf '() num 2)
  )