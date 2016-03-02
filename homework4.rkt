#lang racket
(define (divide-all lst)
  (define (iter tail result)
    (cond [(null? tail) result]
          [(andmap (λ(x)(= 0 (remainder x (car tail)))) lst)
           (iter (cdr tail)(cons (car tail) result))]
          [else (iter (cdr tail) result)]))
  (iter lst '()))

(define (diagonally-dominant-matrix mat)
  (define (iter pos tail)
    (cond [(null? tail) #t]
          [(>= (abs (list-ref (car tail) pos))
               (- (apply + (map abs (car tail)))(abs (list-ref (car tail) pos))))
           (iter (add1 pos)(cdr tail))]
          [else #f]))
  (iter 0 mat))
;два эндмепа, упоролся чтоль