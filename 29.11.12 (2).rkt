#lang racket
;в списке найти такой элемент, на который делятся все остальные (числа натуральные),
;есть матрица требуется проверить, что матрица обладает диаг преоблад (макс в каждой строке на главной дигонали)

(define (infinitecar x)
  (define (help str a b)
  (if (= a (- b 1)) (string-append "ca" str "r")
      (help (string-append "d" str) a (+ b 1))))
  (help "" x 0))

(define (maxmultinlist lst)
  (define (help lst1 lst2 lst3 lst4)
    (if (or (empty? lst3) (empty? (cdr lst3))) 
        (if (> (maxinlist lst2) (maxinlist lst4)) #f (maxinlist lst4)) 
        (if (or (empty? lst1) (empty? (cdr lst1))) 
            (help lst3 lst2 (cdr lst3) lst4)
            (help (cdr lst1) (cons (lcm (car lst1) (cadr lst1)) lst2) lst3 lst4))))
  (help lst '() lst lst))

(define (elemdiag mat1 x y lst mat2)
  (if (= x (length mat2)) lst
      (if (= y x) 
          (elemdiag (cdr mat1) (+ x 1) 0 (cons (caar mat1) lst) mat2)
          (elemdiag (cdr (car mat1)) x (+ y 1) lst mat2))))

(define (daigdom mat)
  (define (help mat1 lst1 lst2)
    (if (empty? mat1) 
        (if (not (= (car lst1) (car lst2))) #f
            (if (empty? lst1) #t
                (help mat1 (cdr lst1) (cdr lst2))))
        (help (cdr mat1) (cons (maxinlist (car mat1)) lst1) lst2)))
  (help mat '() (elemdiag mat 0 0 '() mat)))
      
(define (func lst)
  (define (help lst1 a lst2)
    (if (empty? lst1) a
        (if (not (= (remainder (car lst1) a) 0))
            (help lst2 (car lst1) lst2)
            (help (cdr lst1) a lst2))))
  (help lst (car lst) lst))

(define (maxinlist lst)
  (define (help lst1 n)
                (if (empty? lst1) n
        (if (> (car lst1) n)
            (help (cdr lst1) (car lst1))
            (help (cdr lst1) n))))
  (help lst (car lst)))
