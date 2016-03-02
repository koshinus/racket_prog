#lang racket
(define (num-only f1 f2)
  (define lst (string-split (file->string f1)))
  (define (help lst1 lst2)
    (if (empty? lst1) (to-f2 f2 (reverse lst2))
      (help (cdr lst1) (if (equal? (without-bracket (car lst1)) #f) lst2 (cons #\newline (cons #\return (cons (car lst1) lst2)))))))
  (help lst '()))

(define (to-f2 f lst)
  (define out (open-output-file f #:exists 'replace))
  (define (help lst)
      (if (empty? lst) (close-output-port out)
          (begin (if (equal? (char? (car lst)) #t) (write-char (car lst) out) (write-string (car lst) out)) (help (cdr lst)))))
  (help lst))
;  (write-char #\return) (write-char #\newline)
  
(define (member? x lst)
  (if (empty? lst) 0
   (if (equal? x (car lst)) 1
          (member? x (cdr lst)))))

(define (without-bracket str)
  (define lst (string->list str))
    (if (equal? (number?? (list->string (remove #\) (remove #\( (if (or 
                                                                     (and (= 0 (member? #\. lst)) (= 0 (member? #\) lst)) (= 0 (member? #\( lst)))
                                                                     (and (= 1 (member? #\. lst)) (= 1 (member? #\) lst)) (= 1 (member? #\( lst)))) lst (cons #\y lst)))))) #f) #f str))

(define (number?? str)
  (define (help lst1)
      (if (empty? lst1) #t
          (cond ((equal? (car lst1) #\0) (help (cdr lst1)))
                ((equal? (car lst1) #\.) (help (cdr lst1)))
                ((equal? (car lst1) #\1) (help (cdr lst1)))
                ((equal? (car lst1) #\2) (help (cdr lst1)))
                ((equal? (car lst1) #\3) (help (cdr lst1)))
                ((equal? (car lst1) #\4) (help (cdr lst1)))
                ((equal? (car lst1) #\5) (help (cdr lst1)))
                ((equal? (car lst1) #\6) (help (cdr lst1)))
                ((equal? (car lst1) #\7) (help (cdr lst1)))
                ((equal? (car lst1) #\8) (help (cdr lst1)))
                ((equal? (car lst1) #\9) (help (cdr lst1)))
                (else #f))))
  (help (string->list str)))