#lang racket
(define (similiar lst1 lst2)
  (define (help lst1 lst2 lst3)
    (if (or (empty? lst1) (empty? lst2)) lst3
        (if (= (car lst1) (car lst2)) 
            (help (cdr lst1) (cdr lst2) (cons (car lst1) lst3))
            (if (> (car lst1) (car lst2)) (help lst1 (cdr lst2) lst3)
                (help (cdr lst1) lst2 lst3)))))
  (help (sort lst1 <) (sort lst2 <) '()))

;список, найти сумму имеющихся чисел армстронга

(define (power n)
  (define (help x y)
    (if (= x 0) y
        (help (quotient x 10) (+ y 1))))
  (help n 0))

(define (armstrong? m s n l)
    (if (= n 0) (if (= m s) 1 0)
        (armstrong? m (+ (expt (remainder n 10) l) s) (quotient n 10) l)))

(define (sumnum n m)
  (if (= n 0) m
      (sumnum (quotient n 10) (+ (remainder n 10) m))))

(define (armstrong lst)
  (define (help lst1 sum)
    (define (armstrong? m s n l)
      (if (= n 0) (if (= m s) (if (= (simple (sumnum m 0)) 1) 1 0) 0)
          (armstrong? m (+ (expt (remainder n 10) l) s) (quotient n 10) l)))
    (if (empty? lst1) sum
  (help (cdr lst1) (if (= (armstrong? (car lst1) 0 (car lst1) (power (car lst1))) 1) (+ (car lst1) sum) sum))))
  (help lst 0))

(define (simple n)
  (define (help a b)
    (if (= a 2) 1
      (if (= (remainder a b) 0) b
          (if (> b (sqrt a)) 1
              (help a (+ b 1))))))
  (help n 2))

(define (uniposmat mat f)
  (define (help1 mat1 lst1 k)
    (define (help2 str l lst1)
      (if (empty? str) lst1
          (help2 (cdr str) (+ l 1) (if (f (car str)) (cons (cons k l) lst1) lst1))))
    (if (empty? mat1) lst1
        (help1 (cdr mat1) (+ k 1) (help2 (car mat1) 0 lst1))))
  (help1 mat '() 0))
;список найти самую длинную последловательность, где знаки чередуются (0 нарушает любую последовательность) выдать список
; есть число, определить ближайшее к нему число фибаначчи