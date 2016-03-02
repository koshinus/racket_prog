#lang racket
;domashka
;все элементы списка разные
(define (different lst)
  (define (truefalse lst1 lst2 lst3)
    (if (= (car lst1) (cadr lst2)) #f
        (if (not (empty? lst1)) (truefalse (cdr lst1) lst3 lst3) 
            (if (not (empty? lst2))(truefalse lst3 (cdr lst2) lst3) #t))))
  (truefalse lst lst lst))
;найти произведение элементов списка, у которых первая цифра совпадает с последней
(define (proiz lst)
  (define (help x y z)
    (if (= x 0) (if (= y z) 1 0)
        (help (quotient x 10) y (remainder x 10))))
  (define (f lst1 u s)
    (if (= (help u (remainder u 10) 0) 0)
        (f (cdr lst1) (car lst1) (* s u))
        (f (cdr lst1) u s)))
  (f lst 0 0))