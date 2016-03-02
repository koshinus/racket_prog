#lang racket
;многоугольник определить, есть ли в нем вершина, из которой видны все остальные
(define (task top-list)
  (define (help1 side res)
    (if (null? cdr side) res
        (help1 (cdr side) (filter (λ (point) ((vec-pr (make-vector (car side) (cadr side)) (make-vector (car side) point)) S 0)) res))))
  (define S (- (S top-list)))
  (define (vec-pr v1 v2) (- (* (car v1) (cdr v2)) (* (cdr v1 (car v2)))))
  (define (make-vector x y) (cons (- (car y) (car x)) (- (cdr y) (cdr x))))
  (help1 (cons (car top-list) (reverse top-list))))
;дан треугольник, найти центр описанной окружности
(define (ABC st)
  (define B (- (cdr (cadr st)) (cdr (car st))))
  (define A (- (car (cadr st)) (car (car st))))
  (define C (- (+ (* A (/ (+ (car (cadr st)) (car (car st))) 2)) (* B (/ (+ (cdr (cadr st)) (cdr (car st))) 2)))))
  (list A B C))

(define (2-mid-perp tr)
  (define 2-st (cons (cdr (reverse tr)) (list (cdr tr))))
  (map (λ (x) (ABC x)) 2-st)) 

(define (ccc tr)
  (define (help lst1 lst2)
    (define n (lcm (car lst1) (car lst2)))
    (define a (if (> (* (car lst1) (car lst2)) 0) 
                  (map (λ (x y) (+ (* (/ n (car lst1)) x) (* -1 (/ n (car lst2)) y))) lst1 lst2)
                  (map (λ (x y) (+ (* (/ n (car lst1)) x) (* (/ n (car lst2)) y))) lst1 lst2)))
    (define b (caddr (map (λ (x) (/ x (cadr a))) a)))
    (cons (/ (- (caddr lst1) (* b (cadr lst1))) (car lst1)) b))
  (help (car (2-mid-perp tr)) (cadr (2-mid-perp tr))))
;есть многоугольник построить его выпуклую оболочку  