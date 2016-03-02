#lang racket
(define (reverse-ascending-parts lst)
  (define (iter tail sequence result)
    (cond [(null? tail) (append result sequence)]
          [(> (car tail)(car sequence))(iter (cdr tail)(cons (car tail) sequence) result)]
          [else (iter (cdr tail)(list (car tail))(append result sequence))]))
  (iter (cdr lst)(list (car lst)) null))

(define (derivative Pn)
  (define (iter tail degree result)
    (if (null? tail)
        result
        (iter (cdr tail)(cdr degree)(cons (* (car tail)(car degree)) result))))
  (iter (cdr (reverse Pn)) (cdr (range (length Pn))) null))

(define (i-j-element i j mat)
  (list-ref (list-ref mat i) j))

(define (map-mat function mat)
  (map (λ(x)(map (λ(y)(function y)) x)) mat))

(define (foldl-mat function1 function2 result mat)
  (foldl (λ(row row-res)(function1 (foldl function2 (car row) (cdr row)) result)) result mat))

(define (most-often-element lst)
  (define (iter tail list-of-elements)
    (if (null? tail)
        ()))
  )