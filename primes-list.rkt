#lang racket
(define (primes-list n)
  (let ((lst (build-list n (lambda(x)(+ x 1)))))
  (define (prime? x)
    (define (help y)
      (if (> (* y y) x)
          #t
          (if (zero? (remainder x y))
              #f
              (help (add1 y))
              )
          )
      )
    (help 2)
    )
  (filter prime? lst))
  )