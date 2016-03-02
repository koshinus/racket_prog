#lang racket
(define (max-min ls);у тебя куча постоянных переменных из-за дефайнов, нужно переписать с летами всю эту хурму
  (define (max-min-values max min tail)
    (if (empty? tail)
        (cons max min)
        (cond [(> (car tail) max) (max-min-values (car tail) min (cdr tail))]
              [(< (car tail) min) (max-min-values max (car tail) (cdr tail))]
              [else (max-min-values max min (cdr tail))]
              )
        )
    )
  (define max-min-value-pair (max-min-values (car ls) (car ls) ls))
  (define max-value (car max-min-value-pair))
  (define min-value (cdr max-min-value-pair))
  (define (iter count-max count-min tail)
    (if (empty? tail)
        (cons count-max count-min)
        (cond [(= (car tail) max-value) (iter (add1 count-max) count-min (cdr tail))]
              [(= (car tail) min-value) (iter count-max (add1 count-min) (cdr tail))]
              [else (iter count-max count-min (cdr tail))]
              )
        )
    )
  (define max-min-pair (iter 0 0 ls))
  (cond [(or (= 0 (cdr max-min-pair))(= 0 (car max-min-pair))) "equal"]
        [(= (car max-min-pair)(cdr max-min-pair)) "equal"]
        [(> (car max-min-pair)(cdr max-min-pair)) "max"]
        [(< (car max-min-pair)(cdr max-min-pair)) "min"]
    )
  )