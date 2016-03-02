#lang racket
;создаёт различные сочетания из m по длине списка
(define (comb m lst)
  (cond ((= m 0) '(()))
        ((null? lst) '())
        (else (append (map (λ (y) (cons (car lst) y))
                           (comb (- m 1) (cdr lst)))
                      (comb m (cdr lst))))))
;та самая функция, foldl для кортежей из n элементов из списка
;функция должна принимать n+1 аргументов
(define (foldl-n function start lst n)
  (define combinations (comb n lst))
  (foldl (λ(x y)(apply function y x)) start combinations))
;для числа считает сумму его цифр
(define (digit-sum number)
  (define (iter num sum)
    (if (= 0(quotient num 10))
        (+ sum num)
        (iter (quotient num 10)(+ sum (remainder num 10)))))
  (iter number 0))
;даётся список, выводит позиции элементов с максимальной суммой цифр
(define (max-digit-sum-pos lst)
  (let ((max-list (map digit-sum lst)))
    (reverse (car
     (foldl (λ(element data) ;data - '((list-of-pos) . (max-value . current-pos))
              (cond [(> element (cadr data))
                     (cons (cddr data)(list element (add1 (caddr data))))]
                    [(= element (cadr data))
                     (cons (append (cddr data)(car data))(list element (add1 (caddr data))))]
                    [else
                     (cons (car data)(list (cadr data)(add1 (caddr data))))]
                    )) 
            (cons '()(list (car max-list) 0)) max-list)))))