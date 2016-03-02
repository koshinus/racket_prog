#lang racket
(define (del x y)(if (= (remainder x y)0) ;предикат, проверяющий делимость x на y
                     #t
                     #f))

(define (year x)(if (and 
                     (del x 4)
                     (not (del x 100))) ;ошибка
                    #t 
                    #f))