#lang racket
;дан список, найти числа армстронга
(define (simple n)
  (define (help a b)
    (if (= a 2) 1
      (if (= (remainder a b) 0) b
          (if (> b (sqrt a)) 1
              (help a (+ b 1))))))
  (help n 2))

(define (sumnum n m)
  (if (= n 0) m
      (sumnum (quotient n 10) (+ (remainder n 10) m))))

(define (armstrong lst)
  (define (help lst1 lst2)
    (define (armstrong? m s n)
      (if (= n 0) (if (= m s) (if (= (simple (sumnum m 0)) 1) 1 0) 0)
          (armstrong? m (+ (expt (remainder n 10) 3) s) (quotient n 10))))
    (if (empty? lst1) lst2
  (help (cdr lst1) (if (= (armstrong? (car lst1) 0 (car lst1)) 1) (cons (car lst1) lst2) lst2))))
  (help lst '()))
      

(define (armstrong? m s n)
    (if (= n 0) (if (= m s) #t #f)
        (armstrong? m (+ (expt (remainder n 10) 3) s) (quotient n 10))))  

;ана матрица из чисел 2*к, вывести пару из индексов местоположение счастливого числа

(define (happy x)
  (define (help a b c d e)
    (if (= a 0) 
        (if (= e (/ d 2)) (if (= (sumnum b 0) (sumnum c 0)) 1 0)
          (help a (quotient b 10) (+ (* c 10) (remainder b 10)) d  (+ e 1)))
        (help (quotient a 10) b c (+ d 1) e)))
  (help x x 0 0 0))

(define (happymat mat)
  (define (help mat1 a b lst1 lst2)
    (if (empty? mat1) lst2
        (if (empty? lst1)
            (help (cdr mat1) (+ a 1) 0 (car mat1) lst2)
            (help mat1 a (+ b 1) (cdr lst1) (if (= (happy (car lst1)) 1) (cons (cons a b) lst2) lst2)))))
  (help mat 0 0 (car mat) '()))