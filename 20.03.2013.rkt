#lang racket
(define (trans mat)
  (define (help mat1 mat2)
    (if (empty? (car mat1)) (reverse mat2)
        (help (map cdr mat1) (cons (map car mat1) mat2))))
  (help mat '()))

(define (build-mat n m f)
  (build-list m (λ (x) (build-list n (λ (y) (f x y))))))

(define (build-mat1 n m)
  (build-list n (λ (x) (build-list m (λ (y) (+ x y))))))

(define (map-mat f mat)
  (map (λ (x) (map f (car mat))) mat))

(define (map-mat1 f mat)
  (define (help f mat1 mat2)
    (if (empty? mat1) (reverse mat2)
        (help f (cdr mat1) (cons (map f (car mat1)) mat2))))
  (help f mat '()))

(define (mat-reverse mat)
  (define (help mat1 mat2)
    (if (empty? mat1) (reverse mat2)
        (help (cdr mat1) (cons (reverse (car mat1)) mat2))))
  (help mat '()))

(define (value-in-mat x)
  (define (help x y mat)
    (if (= x y) (reverse mat)
        (help x (+ y 1) (cons (list y) mat))))
  (help x 0 '()))

(define (snake-reverse mat)
  (define (help mat1 mat2 x)
    (if (empty? mat1) mat2
        (help (cdr mat1) (if (= (remainder x 2) 0) (cons (reverse (car mat1)) mat2) (cons (car mat1) mat2)) (+ x 1))))
  (help mat '() 1))

(define (num->str lst)
  (define (help lst1 lst2 lst3 lst4)
    (if (empty? lst1) (cons '("1" #\return #\newline) lst3)
        (if (empty? lst2) (help (cdr lst1) (car lst1) (cons (reverse lst4) lst3) '())
            (if (or (equal? (car lst2) #\return) (equal? (car lst2) #\newline) (equal? (car lst2) #\space)) (help lst1 (cdr lst2) lst3 (cons (car lst2) lst4))
                (help lst1 (cdr lst2) lst3 (cons #\space (cons (number->string (car lst2)) lst4)))))))
  (help (cdr lst) (car lst) '() '())) 

(define (newline-to-mat mat)
  (map (λ (x) (reverse (cons #\newline (cons #\return (reverse x))))) mat))

(define (space-to-mat mat)
  (map (λ (x) map (λ (y) (if (equal? (number? (car y)) #t) (cons #\space y) (cons '() y))) x) mat))

(define (space-to-mat1 mat) 
  (define (help mat1 mat2 lst1 lst2)
    (if (empty? mat1) (reverse mat2)
        (if (empty? lst1) (help (cdr mat1) (cons (reverse lst2) mat2) (car mat1) '())
            (help mat1 mat2 (cdr lst1) (if (equal? (number? (car lst1)) #t) (cons (car lst1) (cons #\space lst2)) (cons (car lst1) lst2))))))
  (help (cdr mat) '() (car mat) '()))

(define (snake-mat n m)
  (define (help mat lst y x z)
    (if (= z x) 
        (to-f "in.txt" (reverse (cons '(1) (reverse (space-to-mat1 (reverse (cons '(1) (reverse (newline-to-mat (trans (snake-reverse mat)))))))))))
        (help (cons (map (λ (a) (+ y a)) lst) mat) (map (λ (a) (+ y a)) (car mat)) y x (+ z 1))))
  (if (= n 1) (to-f "in.txt" (reverse (cons '(1) (reverse (newline-to-mat (add1-mat1 (value-in-mat m)))))))
  (help (list (map add1 (build-list m values))) (map add1 (build-list m values)) m n 1)))

(define (add1-mat1 mat)
  (define (help mat1 mat2)
    (if (empty? mat1) (reverse mat2)
        (help (cdr mat1) (cons (map add1 (car mat1)) mat2))))
  (help mat '()))
        
(define (to-f f lst)
  (define out (open-output-file f #:exists 'replace))
    (define (help lst1 lst2)
      (if (empty? lst1) (close-output-port out)
          (if (empty? lst2) (help (cdr lst1) (car lst1))
              (if (or (equal? (car lst2) #\return) (equal? (car lst2) #\newline) (equal? (car lst2) #\space))
          (begin (write-char (car lst2) out) (help lst1 (cdr lst2)))        
          (begin (write (car lst2) out) (help lst1 (cdr lst2)))))))
  (help (cdr lst) (car lst)))