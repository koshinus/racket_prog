#lang racket
;дан файл, в нем инфа про кроссворд нужно проверить правильность, в каждой строке  указывается слово, гор/верт, позиция с которой начинается 1 буква
(define (length-lst lst n)
  (if (empty? lst) n
      (length-lst (cdr lst) (+ n 1))))

(define (kross? sl1 sl2 x1 x2 y1 y2)
  (define l1 (string-length sl1))
  (define l2 (string-length sl2))
  (define x1 (- x1 x2))
  (define y1 (- y1 y2))
  (not (and (>= x1 0) (< x1 sl1) (>= y1 0) (< y1 sl2) (not (equal? (string-ref sl1 y1) (string-ref sl2 x1))))))

(define (read-data)
  (define in (open-input-file ""))
  (define (read-word sl)
    (define char (read-char in))
    (if (equal? char #\space) (list->string (reverse sl))
        (read-word (cons char sl))))
  (define n (read n))
  (define (read-all s1 s2 n)
    (if (= n 0) (cons s1 s2) (let ((sl (read-word '()))
                                   (flag (read in))
                                   (x (read in))
                                   (y (read in)))
                               (if (= flag 0) (read-all (cons (list sl x y) s1) s2 (- n 1))
                                   (read-all sl (cons (list s1 x y) s2) (- n 1))))))
  (read-all '() '() n))

(define (f lst1 lst2)
  (andmap (λ (a) (andmap (λ (b) (kross? (car a) (car b) (cadr a) (cadr b) (caddr a) (caddr b))) lst2) lst1))
(read-data))

;сформировать файл^ вводится число n заполнить по спирали
                                     