#lang racket
(define (ascending? lst)
  (cond [(or (null? (cdr lst))(null? lst)) #t]
        [(> (cadr lst)(car lst))(ascending? (cdr lst))]
        [else #f]))

(define (number-position-pairs lst)
  (reverse 
   (foldl (λ(element position data)
           (cons (cons element position) data)) null lst (range (length lst)))))

(define (number->list num)
  (define (iter number stack)
    (if (= 0 (quotient number 10))
        (cons (remainder number 10) stack)
        (iter (quotient number 10)(cons (remainder number 10) stack))
        ))
  (iter num null))

(define (power10 pow)
  (define (iter last i)
    (if (= i 0)
        last
        (iter (* last 10)(- i 1))))
  (iter 1 pow))

(define (list->number lst)
  (foldl (λ(digit power result)
           (+ result (* digit (power10 power)))) 0 (reverse lst) (range (length lst))))

(define (mod3 lst)
  (list 
   (filter (λ(x)(= 0 (remainder x 3))) lst)
   (filter (λ(x)(= 1 (remainder x 3))) lst)
   (filter (λ(x)(= 2 (remainder x 3))) lst)))

(define (mod number lst)
  (foldl (λ(rem result)
           (cons (filter (λ(x)(= rem (remainder x number))) lst) result))
         null (range number)))

(define (reducing-fractions pair)
  (let ((g-c-d (gcd (car pair)(cdr pair))))
  (cons (/(car pair) g-c-d)(/(cdr pair) g-c-d))))

