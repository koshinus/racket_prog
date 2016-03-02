#lang racket
(define (sum-by-lists list1 list2)
  ; надо сделать так, чтобы списки разной длины складывало
  ; а так же нормально работало при 5+5 и тому подобном
  (define len1 (length list1))
  (define len2 (length list2))
  (define min-len (min len1 len2))
  ;   есть такая команда, take/drop
  (define (to-10 lst)
    (define (iter stack tail add1?)
      (cond [(null? tail)(reverse stack)]
            [(> 9 (car tail))(iter (cons (- (car tail) 10) stack (cdr tail) 1))]
            []))
   (if (> len1 len2)
       (append (take list1 (- len1 min-len)) (map + (drop list1 (- len1 min-len)) (take list2 len2)))
       (append (take list2 (- len2 min-len))(map + (drop list2 (- len2 min-len)) (take list1 len1)))))