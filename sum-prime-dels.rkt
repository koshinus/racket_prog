#lang racket
(define (factorial n)
  (define (iter n result)
    (if (< n 0)
        0
        (if (= n 0)
        result
        (iter (- n 1) (* result n)))))
  (iter n 1))

(define (prime? x)
  (if (or (= 0 (remainder (+ 1 (factorial (- x 1))) x))(= x 1))
      #t
      #f))

(define (sum-div x)
  (define (sum n div)
    (if (= div x) 
    	n
        (if (= (remainder x div) 0) 
            (sum (+ n div) (+ div 1))
            (sum n (+ div 1))
            )
        )
    )
  (sum 0 1)
  )

(define (sum-div2 x)
  (define sq (sqrt x))
  (define (f1 d s)
    (if (> d sq) 
        (if (= (sqr (- d 1))) 
            (- s (- d 1))
            s
            )
        (if (= 0 (remainder x d)) 
            (f1 (add1 d)(+ s (/ x d)))
            (f1 (add1 d) s)
                         )))
  (f1 1 2))
  