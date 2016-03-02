#lang racket
(define (trail mat)
  (define (help mat1 lst n s)
    (if (= n (length mat1)) lst
        (if (= s n) 
            (help (cdr mat1) (cons (car (car mat1)) lst) (+ n 1) 0)
            (help (cdr (car mat1)) lst n (+ s 1))))
    (map + lst))
  (help mat '() 0 0))

(define (trail1 mat)
  (define (help  tail result)
    (if (null? (car tail)) result
        (help (map cdr (cdr tail) + result (caar tail)))))
  (help mat 0))

(define (maxinlist lst)
  (define (help lst1 n)
                (if (empty? lst1) n
        (if (> (car lst1) n)
            (help (cdr lst1) (car lst1))
            (help (cdr lst1) n))))
  (help lst (car lst)))

(define (maxmat mat)
  (define (help mat1 n)
  (if (empty? mat1) n
      (if (< n (maxinlist (car mat1)))
          (help (cdr mat1) (maxinlist (car mat1)))
          (help (cdr mat1) n))))
  (help mat (car (car mat))))

(define (samesign lst)
  (andmap (lambda (x) (> (* (car lst) x) 0)) lst))

;(define (firstnull lst)
 ; (define (help lst1 x)
  ;(if (empty? lst1) x
   ;   (if (= 0 (car lst))
        

(define (trans mat)
  (define (help mat1 mat2 lst n)
    (if (= (- n 1) (length mat1)) mat2
        (help (list-ref mat1 1) (cons lst mat2) (map (car mat1) mat1) (+ n 1))))
  (help mat '() '() 0))

(define (trans1 mat)
  (define (help tail result)
    (if (null? (car tail)) (reverse result)
        (help (map cdr tail) (cons (map car tail) result))))
  (help mat '()))

(define (mult mat1 mat2)
  (define (scal s1 s2)
    (foldl + 0 (map * s1 s2)))
  (define mat3 (trans1 mat2))
  (if (= (length (car mat1)) (length mat2))
  (map (lambda (str1) 
         (map (lambda (str2) (scal str1 str2)) mat3)) mat1)
  (error "we can't multiplied matricies")))
