#lang racket
(define (copytolist lst1 lst2)
  (if (empty? lst1)  lst2
      (copytolist (cdr lst1) (cons (car lst1) lst2))))

(define (revlist lst)
  (define (help lst1 lst2 lst3)
    (if (empty? (cdr lst1))
        (cons (car lst1) (copytolist lst2 lst3))
        (if (> (car lst1) (cadr lst1)) 
            (help (cdr lst1) (cons (car lst1) lst2) lst3)
            (help (cdr lst1) (cons (car lst1) '()) lst3))))
  (help (reverse lst) '() '() ))