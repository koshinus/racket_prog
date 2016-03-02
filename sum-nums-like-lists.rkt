#lang racket
(define (sum-num-like-lists num-list1 num-list2)
  (define (iter tail1 tail2 add result)
    (cond [(and (null? tail1)(null? tail2))
           (if (= 0 add)
               result
               (cons add result))]
          [(null? tail1)(let ((temp-result (+ add (car tail2))))
                          (if (< 9 temp-result)
                              (iter null (cdr tail2)
                                    (quotient temp-result 10) 
                                    (cons (remainder temp-result 10) result))
                              (iter null (cdr tail2)
                                    0
                                    (cons temp-result result))))]
          [(null? tail2)(let ((temp-result (+ add (car tail1))))
                          (if (< 9 temp-result)
                              (iter (cdr tail1) null
                                    (quotient temp-result 10) 
                                    (cons (remainder temp-result 10) result))
                              (iter (cdr tail1) null
                                    0
                                    (cons temp-result result))))]
          [else (let ((temp-result (+ add (car tail1)(car tail2))))
                          (if (< 9 temp-result)
                              (iter (cdr tail1) (cdr tail2)
                                    (quotient temp-result 10) 
                                    (cons (remainder temp-result 10) result))
                              (iter (cdr tail1) (cdr tail2)
                                    0
                                    (cons temp-result result))))]))
  (iter (reverse num-list1) (reverse num-list2) 0 null))