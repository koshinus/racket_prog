#lang racket
(define (rometoarab str)
  (define lst (string->list str))
  (define (help nlst n s)
    (if (empty? nlst) (+ n s)
        (help (cdr nlst) (car lst) ((if (>= n (car nlst)) + -) s n))))
  (define alst (map (λ (x) (cons (cond ((eq? x #\M) 1000)
                                 ((eq? x #\C) 100)
                                 ((eq? x #\L) 50)
                                 ((eq? x #\X) 10)
                                 ((eq? x #\I) 1)
                                 ((eq? x #\V) 5)) lst))))
    (help (cdr alst) (car alst) 0))

(define (arabtorome x)
  (define romelst '(#\M #\C #\L #\X #\V #\I))
  (define (help x romelst res)
    (if (= x 0) res
        (help (quotient x 10) (cddr romelst) ((let y (remainder x 10)) (cond ((= y 0) res)
                                                                             ((= y 1) (cons (car romelst) res))
                                                                             ((= y 2) (cons (car romelst) (cons (car romelst) res))
                                                                             ((= y 3) res)
                                                                             ((= y 4) (cons (car romelst) (cadr romelst))res)
                                                                             ((= y 5) res)
                                                                             ((= y 6) res)
                                                                             ((= y 7) res)
                                                                             ((= y 8) (cons (cadr romelst) (cons (car romelst) (cons (car romelst)(cons (car romelst) res)))))
                                                                             ((= y 9) (cons (car romelst) cons (caddr romelst) res)))))))
  (help x romelst '())))

(define (saddlepoint mat)
  (define a (map (λ (s) (foldl max (car s) s) mat)))
  (define b (map (λ (n)  (define s (λ (s) (list-ref n s)) mat))) (foldl min (car s) s) (build-list (length (car mat)) values))
                                                                             
                                                                           
                                                                             