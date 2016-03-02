#lang racket
(define (digit-del? number)
  (define (iter last digit)
    (if (= digit 0)
        #f
        (if (= 0 (remainder number digit))
            (if (= last 0)
                #t
                (iter (quotient last 10)(remainder last 10)))
            #f
            ))
    )
  (iter number 1)
  )