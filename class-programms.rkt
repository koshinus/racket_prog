#lang racket
;самопальный foldl
(define (new-foldl function first lst)
  (define (iter variable tail)
    (if (null? tail)
        variable
        (iter (function variable (car tail))(cdr tail))))
  (iter first lst))
;равны ли первый и последний, а надо чтобы все
(define (simm? lst)
  (let ((first (car lst))(last (car (reverse lst))))
    (equal? first last)))
;не мой, более эффективный
(define (sim? lst)
  (andmap equal? lst (reverse lst)))
;есть кучи координат, надо найти самый маленький треугольник в первой четверти
;формула герона, нахуй её
(define (triangle-area point1 point2 point3)
  (let ((vec1 (cons (- (car point1)(car point2))(- (cadr point1)(cadr point2))))
        (vec2 (cons (- (car point3)(car point2))(- (cadr point3)(cadr point2)))))
    (/ (abs (- (* (car vec1)(cdr vec2)) (* (car vec2)(cdr vec1)))) 2.)))

(define (smallest-triangle-area lst)
  (define flt-lst (filter (λ(x)
                        (and 
                         (>= (car x) 0)
                         (>= (cadr x) 0))) lst))
  (foldl-n (λ(S x y z)(min (triangle-area x y z) S)) +inf.0 flt-lst 3)
  )

;перебор n штук
(define (comb m lst)
  (cond ((= m 0) '(()))
        ((null? lst) '())
        (else (append (map (lambda (y) (cons (car lst) y))
                           (comb (- m 1) (cdr lst)))
                      (comb m (cdr lst))))))

(define (foldl-n function start lst n)
  (define combinations (comb n lst))
  (foldl (λ(x y)(apply function y x)) start combinations))
