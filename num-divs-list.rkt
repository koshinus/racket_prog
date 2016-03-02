#lang racket
(define (num-divs-list num);выводит список всех делителей числа, нужны же простые
  (define (iter i ls)
    (if (= i 0)
        ls
        (if (= 0 (remainder num i))
            (iter (- i 1)(cons i ls))
            (iter (- i 1) ls)
            )
     )
   )
  (iter num '())
  )