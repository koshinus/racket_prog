#lang racket
(define (sumnum x s)
  (if (= x 0) s
      (sumnum (quotient (abs x) 10) (+ s (remainder (abs x) 10)))))

(define (maxnum lst)
  (define (help lst1 lst2)
    (if (empty? lst1) (car (sort lst2 >))
            (help (cdr lst1) (cons (sumnum (car lst1) 0) lst2))))
  (help lst '()))

(define (maxnumlist lst)
  (define (help lst1 lst2 a b)
    (if (empty? lst1) lst2
        (if (= (sumnum (car lst1) 0) a)
            (help (cdr lst1) (cons b lst2) a (+ b 1))
            (help (cdr lst1) lst2 a (+ b 1)))))
  (help lst '() (maxnum lst) 0))

(define (f lst)
  (define sum (map s lst))
  (define m (foldl max (car sum) sum))
  (remove* -1
  (map (lambda (x y) (if (= y m) x -1))
       (build-list (length lst) values) sum)))

(define (f lst)
  (define sum (map s lst))
  (define m (foldl max (car sum) sum))
  (foldl (lambda (x st2) (cons (add1 (car st2)) 
                               (if (= x m) st2 (cdr st2)))) '() sum))
                                   

(define (selffoldl func inc lst)
  (define (help x lst1)
    (if (empty? lst1) x
        (help (func (car lst1) x) cdr lst1)))
  (help inc lst))

(define (selffoldl2 func inc lst1 lst2)
  (define (help x lst)
    (if (empty? lst) x
        (help (func (car lst) x) cdr lst)))
  (help inc (cons lst1 lst2)))
 
;(define (maxlist lst)
 ; (define (help lst1 lst2)
  ;  (if (empty? lst1) lst2)))
        
              