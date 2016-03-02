#lang racket
(define (lucky_number number)
  (if (=
       (+ 
        (- number (* 10 (quotient number 10)))
        (- (quotient number 10)(* 10 (quotient number 100)))
        (- (quotient number 100)(* 10 (quotient number 1000))))
       (+ (- (quotient number 1000)(* 10 (quotient number 10000)))
          (- (quotient number 10000)(* 10 (quotient number 100000)))
          (quotient number 100000)))
      #t
      #f))