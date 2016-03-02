#lang racket
(define (inv lst)
  (define (help lst1 lst2 res)
    (if (empty? lst1) (append res lst2)
        (if (>= (car lst1) (car lst2))
            (help (cdr lst1) (cons (car lst1) lst2) res)
            (help (cdr lst1) (list (car lst1)) (append res lst2)))))
  (help (cdr lst) (list (car lst)) '()))

(define (derivative lst)
  (define x (- (length lst) 1))
  (define (help lst1 h lst2)
        (if (empty? lst1) (reverse (cdr lst2))
            (help (cdr lst1) (- h 1) (cons (* (- h 1) (car lst1)) lst2))))
  (help (cdr lst) x (list (* (car lst) x))))

(define (matrix f i j)
  (list-ref (list-ref f i) j))

(define (matmap mat p)
  (map (lambda (x) (map p x)) mat)) 

;найти самый часто встреч элем списка (2 способами: через сортировку или берем элем из списка и формируем пары содержащие число и его частотность)
;есть два натур числа (дробь) ------> целая часть и период (либо период либо дробную часть)