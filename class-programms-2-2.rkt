#lang racket
(define (digit-sum number)
  (define (iter num sum)
    (if (= 0(quotient num 10))
        (+ sum num)
        (iter (quotient num 10)(+ sum (remainder num 10)))))
  (iter number 0))
(define (max-digit-sum-pos lst)
  (define sum-list (map digit-sum lst))
  (define max-element (apply max sum-list))
  ;(define max-element (foldl max (car sum-list)(cdr sum-list)))
  ; 1)
  (remove* '(0)(map (λ(x y)(if (= y max-element)
                            x
                            0)) (range (length lst)) sum-list)))
  ;(foldl (λ(element data)(cons (add1 (car data))
  ;                             (if (equal? element max-element)
  ;                                 data
  ;                                 (cdr data)
  ;                                 ))) '(0) sum-list))