#lang racket
(define (build-mat row colon function)
  ;function - функция двух переменных
  (define (build-new-row n stack)
    (if (= n -1)
        stack
        (build-new-row (sub1 n) (cons (build-list colon (λ(x)(function x n))) stack))))
  (build-new-row (sub1 row) '()))

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
          [(null? tail1) (let ((min-digit-sum (foldl (λ(x y)(if (< (digit-sum x)(digit-sum y)) x y)) (car tail2) tail2)))
                           (iter (cons min-digit-sum stack) '() (remove min-digit-sum tail2)))]
          [(null? tail2) (let ((min-digit-sum (foldl (λ(x y)(if (< (digit-sum x)(digit-sum y)) x y))(car tail1) tail1)))
                           (iter (cons min-digit-sum stack) (remove min-digit-sum tail1) '()))]
          [else (let ((min-digit-sum1 (foldl (λ(x y)(if (< (digit-sum x)(digit-sum y)) x y)) (car tail1) tail1))
                      (min-digit-sum2 (foldl (λ(x y)(if (< (digit-sum x)(digit-sum y)) x y)) (car tail2) tail2)))
                  (if (>_digit-sum min-digit-sum1 min-digit-sum2)
                      ;2
                      (iter (cons min-digit-sum2 stack) tail1 (remove min-digit-sum2 tail2))
                      (iter (cons min-digit-sum1 stack) (remove min-digit-sum1 tail1) tail2)
                  ))]))
  (iter '() list1 list2))

;(define (sum-by-lists list1 list2)
; надо сделать так, чтобы списки разной длины складывало
; а так же нормально работало при 5+5 и тому подобном
;  (define len (min (length list1)(length list2)))
;   есть такая команда, take/drop
;  (map (λ(x y)(+ x y)) list1 list2))