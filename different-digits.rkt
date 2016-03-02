#lang racket
(define (different-digits? num)
  (define (last-digit num) (remainder num 10))
  (define (is-digit? num) (= 0 (quotient num 10)))
  
  (define (iter number digit break)
    (define (trying tail count)
      (cond [(is-digit? tail) (iter (quotient number 10)(last-digit (quotient number 10)) (if (= tail digit)
                                                                                              (add1 count)
                                                                                              count
                                                                                           ))]
            [(= (last-digit tail) digit) (trying (quotient tail 10)(add1 count))]
            [else (trying (quotient tail 10) count)]))
    (cond [(is-digit? number) #t]
          [(= break 2) #f]
          [else (trying number 0)])
    )
  (iter num (last-digit num) 0)
  )