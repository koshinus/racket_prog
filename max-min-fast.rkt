#lang racket
(define (max-min lst)
  (define (iter max min count-max count-min tail)
    (if (empty? tail)
        (cond [(> count-max count-min) "max"]
              [(< count-max count-min) "min"]
              [(= count-max count-min) "equal"])
        (cond [(> (car tail) max)(iter (car tail) min 1 count-min (cdr tail))]
              [(< (car tail) min)(iter max (car tail) count-max 1 (cdr tail))]
              [(= (car tail) min)(iter max min count-max (add1 count-min) (cdr tail))]
              [(= (car tail) max)(iter max min (add1 count-max) count-min (cdr tail))]
              [else (iter max min count-max count-min (cdr tail))])
        )
    )
  (iter (car lst)(car lst) 1 0 lst)
  )