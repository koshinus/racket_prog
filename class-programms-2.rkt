#lang racket
(define (matrix? mat)
  (if (and (list? mat)(list? (car mat)))
      (let ((len (length (car mat))))
        (andmap (λ(x)(equal? (length x) len)) mat))
      #f))

(define (matrix-sum mat1 mat2)
  (map (λ(x y)(map + x y)) mat1 mat2))

(define (first-colon mat)
  (map car mat))
(define (second-colon mat)
  (map cdr map))

(define (trans-mat mat)
  (map list (car mat)(cadr mat)))