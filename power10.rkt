#lang racket
(define (power10 pow)
  (define (iter last i)
    (if (= i 0)
        last
        (iter (* last 10)(- i 1))
        )
    )
  (iter 1 pow)
  )

(define (len x)
  (define (iter last count)
    (if ()
        )
    )
  )