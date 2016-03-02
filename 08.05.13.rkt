#lang racket
;1
(define (member? m lst)
  (if (empty? lst) #f
      (if (equal? m (car lst)) #t
          (member? m (cdr lst)))))

(define (rem-dup lst)
  (define (help lst1 lst2)
    (if (empty? lst1) lst2
        (if (member? (car lst1) lst2) (help (cdr lst1) lst2)
            (help (cdr lst1) (cons (car lst1) lst2)))))
  (help (cdr lst) (list (car lst))))

(define (amount-isolated f)
  (define lst (file->list f))
  (define n (- (car lst) (length (rem-dup (cddr lst)))))
  (if (empty? lst) #f n))
;2

;4
(define (file-char1 f)
  (define in (open-input-file f))
  (define (next lst1)
    (define n (read-line in))
    (if (equal? n eof)  (reverse (cons (string-append (car lst1) "\r") (cdr lst1)))
        (next (cons n lst1))))
  (next '()))

(define (morm lst)
  (define (help lst1 lst2 n)
      (if (empty? lst1) (reverse lst2)
          (if (or (equal? (car lst1) #\space) (equal? (car lst1) #\return)) 
              (help (cdr lst1) (if (= n 0) lst2 (cons n lst2)) (if (= n 0) n 0))
              (help (cdr lst1) lst2 (+ (* n 10) (drug (car lst1)))))))
        (help lst '() 0))

(define (drug x)
  (cond ((or (equal? x #\0) (equal? x "0")) 0)
        ((or (equal? x #\1) (equal? x "1")) 1)
        ((or (equal? x #\2) (equal? x "2")) 2)
        ((or (equal? x #\3) (equal? x "3")) 3)
        ((or (equal? x #\4) (equal? x "4")) 4)
        ((or (equal? x #\5) (equal? x "5")) 5)
        ((or (equal? x #\6) (equal? x "6")) 6)
        ((or (equal? x #\7) (equal? x "7")) 7)
        ((or (equal? x #\8) (equal? x "8")) 8)
        ((or (equal? x #\9) (equal? x "9")) 9)))

(define (del lst1 lst2)
  (if (empty? lst2) lst1
      (del (remove (car lst2) lst1) (cdr lst2))))

(define (main f)
  (define l (map (λ (x) (morm (string->list x)))(file-char1 f)))
  (define (comp-con n graph)
    (define (help2 lst1 rez1)
      (foldl (λ (x rez) (if (member? x rez) rez (help2 (vector-ref graph (- x 1)) (cons x rez)))) rez1 lst1))          
    (define (help1 list-top k)
      (if (null? list-top) k
          (help1 (del list-top (help2 (take list-top 1) '())) 
                 (add1 k))))
    (help1 (range 1 (add1 n)) 0))
  (comp-con (car (car l)) (list->vector (map cdr (cdr l)))))
;5
(define (main1 f)
  (define l (main f))
  (if (> l 1) "граф несвязный" "граф связный"))
;;;;;;;;;;;;;;;;;;;
(define (comp-con n graph)
    (define (help2 lst1 rez1)
      (foldl (λ (x rez) (if (member? x rez) rez (help2 (vector-ref graph (- x 1)) (cons x rez)))) rez1 lst1))          
    (define (help1 list-top k)
      (if (null? list-top) k
          (help1 (del list-top (help2 (take list-top 1) '())) 
                 (add1 k))))
    (help1 (range 1 (add1 n)) 0))
;4
(define (artic-point f)
  (define l (map (λ (x) (morm (string->list x)))(file-char1 f)))
  (define p (length (cdr l)))
  (define m (main f))
  (define g (list->vector (map cdr (cdr l))))
  (define (help x lst y z)
    (if (= x p) y
      (help (+ x 1) (if (= x 0) '() (vector-ref g z))
            (if (= (comp-con (car (car l)) 
                             (begin (if (= x 0) g (vector-append (vector-take g z) (vector lst) (vector-drop g (+ z 1)))) 
                                    (vector-append (vector-take g x) (vector '()) (vector-drop g (+ x 1))))) m) y (+ y 1)) (+ z 1))))
  (help 0 '() 0 -1))