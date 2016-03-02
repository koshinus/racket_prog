#lang racket
(define (formatter string list)
  (define (iter tail result)
    
    (define (insertion)
      (define (handling tail stack)
        (if (equal? (car tail) #\%)
            (if (null? stack)
                (iter (cdr tail) (cons #\% result))
                (iter (cdr tail) (append (reverse 
                                          (string->list (list-ref list (sub1 (pos-handler stack)))))
                                         result)))
            (handling (cdr tail)(cons (car tail) stack))))
      (handling (cdr tail) null))
    
    (define (pos-handler lst)
      (define (char->num char) (- (char->integer char) 48))
      (if (null? lst) 0 (+ (char->num (car lst))
                           (* 10 (pos-handler (cdr lst))))))
  
  (if (null? tail)
      (list->string(reverse result))
      (if (equal? (car tail) #\%)
          (insertion)
          (iter (cdr tail)(cons (car tail) result)))))
  
  (iter (string->list string) null))