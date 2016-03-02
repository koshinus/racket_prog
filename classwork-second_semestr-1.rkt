#lang racket
(define (common-elements list1 list2)
  (define (iter tail1 tail2 result)
    (cond [(or (null? tail1)
               (null? tail2)) result]
          [(equal? (car tail1)
                   (car tail2)) (iter (cdr tail1)(cdr tail2)(cons (car tail1) result))]
          [(> (car tail1)(car tail2)) (iter tail1 (cdr tail2) result)]
          [else (iter (cdr tail1) tail2 result)]
          ))
  (iter (sort list1 <)(sort list2 <) null))

(define (arm-sum lst)
  (define (number->list num)
    (define (iter num result)
      (if (= 0 (quotient num 10))
          (cons num result)
          (iter (quotient num 10)(cons (remainder num 10) result))))
    (iter num null))
  (define (armstrong? num)
    (define (num-len num)
      (define (iter num result)
        (if (= 0 (quotient num 10))
            result
            (iter (quotient num 10)(add1 result))))
      (iter num 1))
    (let ((len (num-len num)))
      (= num (foldl + 0 (map (λ(x)(expt x len)) (number->list num))))
      ))
  
  (define (prime? num)
    (define (iter tail)
      (if (null? tail) #t
          (if (= 0 (remainder num (car tail))) #f
              (iter (cdr tail)))))
    (iter (cddr (range (add1 (integer-sqrt num))))))
  
  (foldl (λ(element data)(if (and (armstrong? element)
                                  (prime? (foldl + 0 (number->list element))))
                             (+ data element)
                             data)) 0 lst))