#lang racket
(define (file-upcase f1 f2)
  (define in (open-input-file f1))
  (define out (open-output-file f2))
  (define (help)
    (define x (read-char in))
    (if (equal? x eof) (close-output-port out)
        (begin (write-char (char-upcase x) out) (help))))
  (help))

(define (point-in-list? lst)
  (if (equal? (car lst) #\.) 1
    (if (empty? (cdr lst)) (if (equal? (car lst) #\.) 1 0) (point-in-list? (cdr lst)))))

(define (same-long? lst a)
  (if (= a 0) (same-long? (cons (substring (car lst) 0 (- (string-length (car lst)) 1)) (cdr lst)) (+ a 1))
    (if (empty? (cdr lst)) 1 
      (if (not (= (string-length (car lst)) (string-length (cadr lst)))) 0 (same-long? (cdr lst) a)))))

(define (point-in-sentences lst1 lst2)
  (if (empty? lst1) (reverse lst2)
         (point-in-sentences (cdr lst1) (if (= (point-in-list? (string->list (car lst1))) 1) (cons 1 lst2) (cons 0 lst2)))))

(define (equal-word-in-sentence? f)
  (define (help lst2 lst3 lst4 lst5)
        (if (empty? lst3) lst5
          (if (= (car lst3) 0) (help (cdr lst2) (cdr lst3) (cons (car lst2) lst4) lst5)
              (help (cdr lst2) (cdr lst3) '() (if (= (same-long? (cons (car lst2) lst4) 0) 1) (cons (cons (car lst2) lst4) lst5) lst5)))))
  (help (string-split (file->string f)) (point-in-sentences (string-split (file->string f)) '()) '() '()))