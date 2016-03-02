#lang racket
(define (max-in-mat mat)
  (apply max (map (λ(x)(apply max x)) mat)))

(define (max-in-mat2 mat)
  (define (max-in-row row)
    (apply max row))
  (max-in-row (map max-in-row mat)))

(define (same-sign? lst)
  (or (andmap positive? lst)(andmap negative? lst)))

(define (first-zero lst)
  (define (iter tail pos)
    (cond [(null? tail) pos]
          [(= 0 (car tail)) pos]
          [else (iter (cdr tail)(add1 pos))]))
  (if (member 0 lst)
      (iter lst 0) 
      #f))
