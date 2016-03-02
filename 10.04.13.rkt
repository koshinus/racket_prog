#lang racket
(define (value-of-pol lst1 n m)
  (if (empty? lst1) m
      (value-of-pol (cddr lst1) n (+ m (* (car lst1) (expt n (cadr lst1)))))))

(define (to-lst2 lst)
  (define (help lst1 lst2)
    (if (equal? (car lst) #\return) (reverse lst2)
        (help (cdr lst1) (cons (car lst1) lst2))))
  (help lst '()))

(define (to-lst1 lst)
    (if (equal? (car lst) #\return) (cddr lst)
        (to-lst1 (cdr lst))))

(define (to-f f lst)
  (define out (open-output-file f #:exists 'replace))
  (define (help lst)
      (if (empty? lst) (close-output-port out)
          (begin (display (car lst) out) (help (cdr lst)))))
  (help lst))

(define (polinom f)
  (define (help f lst1 lst2 lst3)
    (if (empty? lst1) (to-f f lst3)
        (if (empty? lst2) (help f (to-lst1 lst1) (to-lst2 lst1) lst3)
            (help f lst1 '() (cons (value-of-pol (cdr lst2) (car lst1) 0) (cons #\space lst3))))))
  (help "c:\\Racket\\Программы\\in2.txt" (file->list f) '() '()))

;
(define (vector-remove vec elem)
  (cond ((= elem 1) (vector-drop vec elem))
        ((= elem 2) (vector-append '#(1) (vector-drop vec elem)))
        (else  (help vec (- elem 1)))))
  (define (help vec elem)
    (vector-append (vector-drop-right vec (- (vector-length vec) elem)) (vector-drop vec  (+ elem 1))))
  
(define (counting x y)
  (define (help vec n l m)
    (if (= (vector-length vec) 1) (vector-ref vec 0)
        (if (or (= m (vector-length vec)) (> m (vector-length vec))) (help vec n (+ l 1) 0)
            (if (or (= l n) (> l n)) (help (vector-remove vec (vector-ref vec m)) n 0 (+ m 1))
                (help vec n (+ l 1) (+ m 1))))))
  (help (build-vector x add1) y 0 0))