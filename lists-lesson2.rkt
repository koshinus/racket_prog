#lang racket
;(define (sorted? lst p)
;  (or (empty? lst)
;      (empty? (cdr lst))
;      (and (p (car lst) (cadr lst)) (sorted? (cdr lst) p))))

;сортировки важны, так как есть аналогичные алгоритмы
;стандартная функция

;упорядочивающая функция
;(sort <список> <предикат> [опциональные параметры <#:key - ключевое значение <значение>>])

;пример - (sort '(("abc")("def")("cow"))) #:key car string<?)
; // сортировка по ключу

;remove - удаляет первое вхождение
;remove* - удаляет все вхождения первого списка в другом
;member - возвращает список, начиная с нужного элемента, иначе ложь
;memf - то же, но нужный - первый удовлетворяющий предикату
;findf - то же, но возвращающий только элемент
;assoc - находит список в списке, где в голове внутреннего списка лежит нужный элемент

;Максимальный элемент
;(foldl max -inf.0 '(3 54 23 4 -5 2))

; Примеры:

(define (+- lst)
  ;делит список на два - с положительными и отрицательными
  (cons
   (filter positive? lst)
   (filter (lambda(x)(< x 0)) lst)
   )
  )


(define (position-max lst)
  ;позиция максимума в списке
  ;(максимум, длина, позиция(c нуля))
  (foldl (λ (x y)
           ;сверяет новый элемент x из списка
           ;с сохранённой парой y, она задаёт-
           ;ся через первичный элемент
           (if (> x (car y))
               (list x (add1 (cadr y))(cadr y))
               (list (car y)(add1 (cadr y))(caddr y)))
           )
         (list (car lst) 0 0) lst))

;подсчитывает количество максимумов в списке
(define (count-max lst)
  (define m
    (foldl max (car lst) lst))
  (count (λ (x)(equal? x m)) lst))

;альтернативно за один проход
(define (count_max lst)
  ;(максимум . колличество)
  (foldl (λ (x y)
           (if (> x (car y))
               (cons x 1)
               (if (= x (car y))
                   (cons (car y)(add1 (cdr y)))
                   y)))
         (cons (car lst) 0)
         lst))