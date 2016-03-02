#lang scheme
(define (different lst)
  (sort lst <)
  (if (or (empty? lst) (empty? (cdr lst))) #t
      (if (= (car lst) (cadr lst)) #f
          (different (cdr lst)))))

(define (help x a)
  (if (= x 0) a
      (help (quotient x 10) (+ a 1))))

(define (pow y z b c)
  (if (= (- b 1) c) y
      (pow (* y z) z b (+ c 1))))

(define (proiz lst)
  (define (multip lst1 S)
    (if (empty? lst1) S
        (if (= (remainder (car lst1) 10) (remainder (car lst1) (pow 10 10 (help (car lst1) 0) 0)))
            (multip (cdr lst1) (* S (car lst1)))
            (multip (cdr lst1) S))))
  (multip lst 1))


(define (povt? lst)
  (if (empty? lst) #f
      (if (memq (car lst) (cdr Lst)) #t
          (povt? (cdr lst)))))