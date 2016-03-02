#lang racket
;дан список переставить элементы так, чтобы у соседних элементов НОК был минимальным;
;(define (help1 lst)
;(foldl + 0 (map (λ (x) (lcm x) (car lst) (cadr lst)) lst)))

(define (max-lcm lst)
  (define (sum lst)
    (define (help1 lst s)
      (if (empty? (cdr lst)) s
          (help1 (cdr lst) (+ s (lcm (car lst) (cadr lst))))))
    (help1 lst 0))
  (define (f1 lst lst1 rez)
    (if (empty? (cdr lst)) (if (< (sum (cons (car lst) lst1)) (sum rez)) (cons (car lst) lst1) rez)
        (foldl (λ (i r) (define x (f1 (append (take lst i) (drop (cdr lst) i)) (cons (list-ref lst i) lst1) r))
                          (if (< (sum x) (sum r)) rez (range (length lst)))))))
  (f1 lst '() lst))

(define (F n)
  (define (add-digit n c d))
  (define (dist_2 n))
  (foldl (λ (x))))
;дано натуральное число, переставить цифры так, чтобы получить как можно ближе к простому числу
    