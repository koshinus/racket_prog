#lang racket
(define (matrix? x)
  (if (and (list? x) (list? (car x)))
      (let ((y (length (car x))))
           (andmap (lambda (lst)
                     (and (list? lst) (= (length lst) y))))) x) #f)

(define (mat-sum mat1 mat2)
  (map (lambda (x y) (map + x y))
       mat1 mat2))
;транспонирование, умножение и след 
(define (trans mat)
  (define (iter mat1 mat2)
    (if (empty? mat1) mat2
        (iter (cdr mat1 mat2))))
  (iter mat '()))

(define (mat-mult mat1 mat2)
  (map (lambda (x y) (map + x y))
       mat1 mat2))

(define (trail mat)
  (define (iter mat1 S a b c)
    (if (= a b) S
        (if (= c b) 
            (iter (cdr mat1) (+ S (car (car mat1))) a (+ b 1) 0)
            (iter (cdr (car mat1)) S a b (+ c 1)))))
  (iter mat 0 (length mat) 0 0))


