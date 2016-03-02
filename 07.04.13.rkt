#lang racket
(define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree entry left right)
    (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0) (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))
;2) Сформировать список всех его листьев.

(define (all-leaf1 tree)
  (define (help lst tree)
    (if (empty? tree) lst
        (if (and (empty? (cadr tree)) (empty? (caddr tree))) (help (cons (car tree) lst) '())
            (cond
              ((and (not (empty? (cadr tree))) (empty? (caddr tree))) (help (append (all-leaf1 (cadr tree)) lst) '()))
              ((and (empty? (cadr tree)) (not (empty? (caddr tree)))) (help (append (all-leaf1 (caddr tree)) lst) '()))
              ((and (not (empty? (cadr tree))) (not (empty? (caddr tree)))) (help (append (append (all-leaf1 (cadr tree)) (all-leaf1 (caddr tree))) lst) '()))))))
  (help '() tree))
;
(define (all-leaf tree)
  (define (help lst1 lst2 lst3)
    (if (and (null? (cadr lst1)) (null? (cadr lst2)))
        (remove 0 (complete (car (cons (caddr lst1) (cons (caddr lst2) lst3))) (cadr (cons (caddr lst1) (cons (caddr lst2) lst3)))))
        (help (cdr lst1) (cdr lst2) lst3)))
  (help (cadr tree) (caddr tree) '()))

(define (complete lst1 lst2)
  (define (help lst1 lst2 lst3 lst4)
    (list (if (empty? lst1) 0 (car lst1)) 
               (if (empty? lst2) 0 (car lst2)) 
               (if (empty? lst3) 0 (car lst3)) 
               (if (empty? lst4)  0 (car lst4))))
  (help (cadr lst1) (caddr lst1) (cadr lst2) (caddr lst2)))
;3) В список должны попасть только те узлы, которые имеют детей с четной разрядностью.
(define (amount n)
  (define (help n s)
    (if (= n 0) s
        (help (quotient n 10) (+ s 1))))
  (help n 0))

(define (even? x)
(if (= (remainder (amount x) 2) 0)
#t #f))

;основная программа
(define (child-even tree)
  (define (help lst tree)
    (if (empty? tree) lst
        (if (and (empty? (cadr tree)) (empty? (caddr tree))) lst
            (cond ((and (not (empty? (cadr tree))) (not (empty? (caddr tree)))) 
                   (if (or (even? (car (cadr tree))) (even? (car (caddr tree))))
                       (help (append (cons (car tree) lst) (child-even (cadr tree)) (child-even (caddr tree))) '())
                       (help (append (child-even (cadr tree)) (child-even (caddr tree))) '())))
                  ((and (not (empty? (cadr tree))) (empty? (caddr tree)))
                   (if (even? (car (cadr tree))) (help (append (cons (car tree) lst) (child-even (cadr tree))) '()) lst))
                  ((and (empty? (cadr tree)) (not (empty? (caddr tree))))
                   (if (even? (car (caddr tree))) (help (append (cons (car tree) lst) (child-even (caddr tree))) '()) lst))))))
  (help '() tree))

;4) Проверить дерево на сбалансированность.
(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
    (if (null? tree) result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
(copy-to-list tree '() ))

(define (balance-tree? tree)
  (define (help lst n m l)
    (if (empty? lst) (if (> (abs (- m n)) 4) #f #t)
        (if (pair? (car lst)) (help (cdr lst) n m 1)
            (if (= l 0) (help (cdr lst) (+ n 1) m l)
                (help (cdr lst) n (+ m 1) l)))))
  (help (tree->list-2 (cons (cons (car tree) 1) (cdr tree))) 0 0 0))