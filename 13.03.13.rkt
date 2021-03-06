#lang racket
;дана строка, нужно найти хотя бы один элемент во всех словах или сказать что таких нет
(define (same lst1 lst2)
  (map (λ (x) (member? x lst2)) lst1))

(define (letter lst1 lst2)
  (if (empty? lst1) #f
      (if (= (car lst1) 1) (car lst2)
          (letter (cdr lst1) (cdr lst2)))))

(define (for2case lst1 lst2 lst3)
  (if (empty? lst1) #f
      (if (member? (car lst1) lst2) (car lst1)
          (for2case (cdr lst1) lst3 lst3))))

(define (member? x lst)
  (if (empty? lst) 0
   (if (equal? x (car lst)) 1
          (member? x (cdr lst)))))

(define (symbol-in-string str)
  (define l (string-split str))
  (cond ((= (length l) 0) #f) 
        ((= (length l) 1) (car (string->list (car l))))
        (else (begin (define u (string->list (car l))) 
                     (define l1 (map string->list (cdr l)))
                     (define m (letter (same u (car l1)) u))
        (help u l1 m)))))
  (define (help lst1 lst2 m)
    (if (empty? lst2) m
        (if (equal? m #f) #f
            (if (= (member? m (car lst2)) 1) 
                (help lst1 (cdr lst2) m) #f))))

;3. Функция получает на вход имя файла и целое число n (n > 0). Она должна выдать строки треугольника Паскаля с нулевой по n-ю. Строка с номером k содержит k + 1 натуральное число; крайние _ единицы, каждое внутреннее есть сумма двух чисел предыдущей строки: стоящего над ним и левее него. Нулевая строка состоит только из единицы. Первые несколько строк треугольника Паскаля:
;1
;1 1
;1 2 1
;1 3 3 1
;1 4 6 4 1
(define (to-f f lst)
  (define out (open-output-file f #:exists 'replace))
    (define (help lst1 lst2)
      (if (empty? lst1) (close-output-port out)
          (if (empty? lst2) (help (cdr lst1) (car lst1))
              (if (or (equal? (car lst2) #\return) (equal? (car lst2) #\newline) (equal? (car lst2) #\space))
          (begin (write-char (car lst2) out) (help lst1 (cdr lst2)))        
          (begin (write-string (car lst2) out) (help lst1 (cdr lst2)))))))
  (help (cdr lst) (car lst)))

(define (num->str lst)
  (define (help lst1 lst2 lst3 lst4)
    (if (empty? lst1) (cons '("1" #\return #\newline) lst3)
        (if (empty? lst2) (help (cdr lst1) (car lst1) (cons (reverse lst4) lst3) '())
            (if (or (equal? (car lst2) #\return) (equal? (car lst2) #\newline) (equal? (car lst2) #\space)) (help lst1 (cdr lst2) lst3 (cons (car lst2) lst4))
                (help lst1 (cdr lst2) lst3 (cons #\space (cons (number->string (car lst2)) lst4)))))))
  (help (cdr lst) (car lst) '() '())) 

(define (pascal-triangle f n)
    (cond ((= n 0) (to-f f '(1)))
           ((= n 1) (to-f f '(1))))
  (define (help lst1 lst2 lst3 x y)
        (if (= x y) (to-f f (num->str lst1))
            (if (empty? (cdr lst2)) 
                (help (cons (reverse (cons #\newline (cons #\return (cons 1 lst3)))) lst1) (cons 1 lst3) '(1) x (+ y 1))
                (help lst1 (cdr lst2) (cons (+ (car lst2) (cadr lst2)) lst3) x y))))
  (help '((1 1 #\return #\newline) (1 #\return #\newline)) '(1 1) '(1) n 0))