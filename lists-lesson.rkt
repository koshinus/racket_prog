#lang racket
;списки, пары
(define (swap tuple);меняет порядок в паре
  (cons (cdr tuple)(car tuple))
  )

(define (first l);номер первого чётного элемента
  (define (iter l-n i)
    (if (empty? l-n)
        -1
        (if (= 0 (remainder (car l-n)2))
            i
            (iter (cdr l-n)(+ i 1))
            )
        )
    )
  (iter l 0)
  )

(define (growth? l); идут ли в порядке возрастания 
  (if (or (empty? l)(empty? (cdr l)))
      #t
      (if (> (car l)(cadr l))
          #f
          (growth? (cdr l)))
      )
  )

(define (num-dig->list num);делает из числа список
  (define (iter n ls)
    (if (= n 0)
        ls
        (iter (quotient n 10)(cons (remainder n 10)ls))
     )
    )
  (iter num '())
)

(define (list->num ls);делает из списка число
  (define (iter ls-n n)
    (if (empty? ls-n)
        n
        (iter (cdr ls-n)(+ (* n 10)(car ls-n)))
        )
    )
  (iter ls 0)
  )

(define (num-sist st num);переводит число в любую систему счисления
  (define (iter ls-n n)
    (if (= n 0)
        ls-n
        (iter (cons (remainder n st)ls-n)(quotient n st))
        )
    )
  (iter '() num)
  )

(define (sum-dig num);сумма цифр числа
  (define (iter x s)
    (if (= 0 x)
        s
        (iter (quotient x 10)(+ s (remainder x 10)))
        )
    )
  (iter num 0)
  )

(define (scal* a b);скалярное произведение
  (foldl + 0 (map * a b))
  )

(define (scal2 a b);второй вариант
  (foldl (lambda(x y s)(+ s (* x y))) 0 a b)
  )

(define (do lst)
  (scal* lst (build-list (length lst) add1))
  )