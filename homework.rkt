#lang racket
;первая задача - перемножить все элементы, у которых совпадают первая и последняя цифры
(define (first-digit num)
  (define (iter i)
    (if (= 0 (quotient i 10))
        i
        (iter (quotient i 10))))
  (iter num))

(define (first-and-last-digit-equal? num)
  (equal? (first-digit num)(remainder num 10)))

(define (first-last-dig-product lst)
  (let ((lst-flt (filter first-and-last-digit-equal? lst)))
    (foldl * 1 lst-flt)))

;вторая задачи - проверить, все ли элементы в списке разные
(define (different? lst)
  (define (iter tail element)
    (let ((i (foldl 
              (λ(x y)(if (equal? x element)
                         (cons element (add1 (cdr y)))
                         (cons element (cdr y)))
                )
              (cons element 0)
              lst)))
      ;(элемент . колличество)
      (cond [(empty? tail) #t]
            [(> (cdr i) 1) #f]
            [else (iter (cdr tail)(car tail))])))
  (if (null? lst)
      #f
      (iter lst (car lst))))
;вторая реализация
(define (different2? lst)
  (define (dublicates-list lst)
    (define (iter result tail)
      (cond [(or (null? (cdr tail))(null? tail)) result]
            [(equal? (car tail)(cadr tail)) (iter (cons (car tail) result)(cdr tail))]
            [else (iter result (cdr tail))]
            ))
    (if (pair? lst)
        (iter '() lst)
        (list lst)))
  (let ((dublicates (dublicates-list (sort lst <))))
    (null? dublicates)))

;вариант из класса
(define (diff? lst)
  (cond [(empty? lst) #t]
        [(memq (car lst)(cdr lst)) #f]
        [else  (diff? (cdr lst))]))