#lang racket
#|Есть натуральное число, надо добавить в него куда-нибудь какую-нибудь цифру, чтобы
 получилось как можо ближе к степени двойки.|#
(define (closer-to-2-power num)
  (define (closety-to-pw2 num)
    (define (iter x)
      (if (>= x num)
          (min (- x num)(- num (/ x 2)))
          (iter (* x 2))))
    (iter 2))
  (define (length-num num)
    (if (= 0 (quotinent num 10)
           1
           (+ 1 (length-num (quotient num 10))))))
  
  (define (bruteforce num1 num2)
    (for-each (λ(x)(let ((n (+ num2 (+ (* num2 (expt 10 (add1 (length-num num1)))) x))))
                       (cond [(< (closety-to-2-power n)
                             (closety-to-2-power result))
                          (set! result n)]))) (range 9)))
  
  (define result num)
  (bruteforce num 0)
  result
  )
#|Домашка - дано натуральное число, цифры в нём переставить так, чтобы получить как можно ближе к простому. 
 Останавливать перебор, если получили простое. |#