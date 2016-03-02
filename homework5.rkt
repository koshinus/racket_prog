#lang racket
;нужно в списке выделить возрастающие части, и развернуть их
(define (reverse-ascending-parts lst)
  (define (all-ascending-parts lst)
    (define (iter tail curr-list result)
      (cond [(null? tail) (cons curr-list result)]
            [(> (car tail)(car curr-list))
             (iter (cdr tail)(cons (car tail) curr-list) result)]
            [else (iter (cdr tail) (list(car tail)) (cons curr-list result))]))
    (iter (cdr lst) (list(car lst)) null))
  (apply append (reverse (all-ascending-parts lst))))