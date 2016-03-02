#lang racket
(define (selffoldl func inc lst)
  (define (help x lst)
    (if (empty? lst) x
        (help (func (car lst) x) cdr lst)))
  (help inc lst))

(define (foldl-n d start n lst))

(define (sim lst)
  (if (andmap (equal? lst (reverse lst)))))

(define (Square A B C)
  (define (make-vektor A B)
    (cons (- (car B) (car A)) (- (cadr B) (cadr A))))
  (define AB (make-vector A B))
    (define AC (make-vector A C))
  (abs (/ (- (* (car AB) (cadr AC)) (* (cadr AB) (car AC))) 2)))

(define (minsq lst)
  (define lst (fiter (lambda(a) (>= (and (car a) (cadr a)) 0)) lst))
  (foldl-n (lambda (A B C S) ((min S (Square A B C))) (inf 0 3) lst)))