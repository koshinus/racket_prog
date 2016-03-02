#lang racket
(define (member? x lst)
  (if (empty? lst) #f
      (if (equal? (car lst) x) #t
          (member? x (cdr lst)))))

(define (file-char)
  (define in (open-input-file "in.txt") )
  (define (next)
    (define n (read-line in))
    (if (equal? n eof) (write 'end) (next)))
  (next))

(define (del lst1 lst2)
  (if (empty? lst2) lst1
      (del (remove (car lst2) lst1) (cdr lst2))))

(define (comp-con n graph)
  (define (iter2 lst1 rez1)
    (foldl (λ(x rez) (if (member? x rez) rez (iter2 (vector-ref graph (- x 1)) (cons x rez)))) rez1 lst1))           
  (define (iter1 list-top k)
    (if (null? list-top) k
        (iter1 (del list-top (iter2 (take list-top 1) '())) 
               (add1 k))))
  (iter1 (range 1 (add1 n)) 0))

(define (iter2 lst1 rez1 graph)
    (foldl (λ(x rez) (if (member? x rez)
                         rez
                         (iter2 (vector-ref graph (- x 1))
                                (cons x rez)
                                graph)))
           rez1 lst1))