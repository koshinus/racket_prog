#lang racket
(define (digit-sum number)
  (define (iter num sum)
    (if (= 0(quotient num 10))
        (+ sum num)
        (iter (quotient num 10)(+ sum (remainder num 10)))))
  (iter number 0))

(define >_digit-sum
  (λ(x y)(> (digit-sum x)(digit-sum y))))

(define (2-list-sort-sum-digit list1 list2)
  (define (iter stack tail1 tail2)
    (cond [(and (null? tail2)(null? tail1)) (reverse stack)]
          [(null? tail1)(iter (cons (car tail2) stack) '() (cdr tail2))]
          [(null? tail2)(iter (cons (car tail1) stack) (cdr tail1) '())]
          [else (if (>_digit-sum (car tail1) (car tail2))
                    ;2
                    (iter (cons (car tail2) stack) tail1 (cdr tail2))
                    (iter (cons (car tail1) stack) (cdr tail1) tail2)
                    )]))
  (iter '() list1 list2))