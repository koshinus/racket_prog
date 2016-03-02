#lang racket
;дано несколько окружностей, даны координы центров окружности, радиусы (у всех одинаковы) найти длину ниточки
;дано четное количество точек, нужно выбрать 2 точки, проведя прямую через которые, мы получим одинаковое количество точек с той и с другой стороны прямой
;файл с кучей чисел, нужно найти медиану
(define (med f)
  (define in (open-input-file))
  (define vec (make-vector 1001 0))
  (define (next k)
    (define n (read in))
    (if (equal? n eof) k
        (begin (vector-set! vec n (+ 1 (vector-ref vec n))) (next (+ k 1)))))
  (define (help k sum)
    (if (>= sum (/ k 2))
        (if (= (remainder k 2) 0) n 
;