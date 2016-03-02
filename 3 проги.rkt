#lang racket
(define (more lst)
  (define (more-max-min lst1 x y a b)
    (if (empty? lst1) (if (> b a) b a)
        (if (> (car lst1) x) (more-max-min (cdr lst1) (car lst1) y 1 b)
            (if (< (car lst1) y) (more-max-min cdr (lst1 x car) lst1 a 1)
                (if (= (car lst1) x) (more-max-min (cdr lst1) x y (+ a 1) b)
                    (if (= (car lst1) y) (more-max-min (cdr lst1) x y a (+ b 1))
                        (more-max-min (cdr lst1) x y a b)))))))
  (more-max-min lst 0 0 0 0))


(define (simple x y z)
  (if (> y x) (if (> z 2) 0 1)
      (if (= (remainder x y) 0) (simple x (+ y 1) (+ z 1)) (simple x (+ 1 y) z))))


(define (spisok n)
  (define (good lst a b)
    (if (= a 1) lst
        (if (= (simple b 1 0) 0) (good lst (+ b 1) a)
            (if (= (remainder a b) 0)
                (good (cons b lst) b (/ a b))
                (good lst (+ b 1) a)))))
  (good '() n 1)) 
            

(define (simplesps n)
  (define (iter s x d)
      (if (> (* d d) x) (cons x s)
          (if (= (remainder x d) 0)
              (iter (cons d s) (/ x d) d)
              (iter s x (+ 1 d)))))
  (iter '() n 2))

(define (arifm lst)
  (define (t lst1 lst2)
    (define s 
      (filter (lambda (x) (= (remainder x 3) 0) lst)))
    (define m (length s))
    (if (= m 0) "список пуст" (/ (foldl + 0 s) m))))


(define (dist n)
  (define (iter x)
    (if (>= x n)
        (min (- x n) (- n (/ x 2)))
        (iter (* x 2))))
  (iter 2))


(define (F lst)
  (define newlst (map dist lst))
  (define M (foldl min (car newlst))
    newlst)
  (define (Num x lst p)
  (if (= (car lst) x) p
      (Num x (cdr lst) (+ p 1))))
  (Num M newlst 0))