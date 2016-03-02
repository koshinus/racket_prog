#lang racket
(define (most-often-element lst)
  (define (iter tail element count max-element max-count)
    (cond [(null? tail) (if (> max-count count)
                            (cons max-element max-count)
                            (cons element count))]
          [(equal? (car tail) element)
           (iter (cdr tail) element (add1 count) max-element max-count)]
          [else (if (> count max-count)
                    (iter (cdr tail) (car tail) 1 element count)
                    (iter (cdr tail) (car tail) 1 max-element max-count))]))
  (let ((sorted-lst (sort lst >)))
    (iter sorted-lst (car sorted-lst) 0 (car sorted-lst) 0)))

(define (integral-fractional-parts num)
  (define (iter stack rest)
    (define current-value (floor(* rest 10)))
    (cond [(= 0 current-value) stack]
          [else (iter (append stack (list current-value)) (- (* rest 10)(floor (* rest 10))))])
    )
  (let* ((int-part (floor num))(frac-part (- num int-part)))
    (cons int-part
          (iter (list (floor (* 10 frac-part))) (- (* frac-part 10) (floor (* frac-part 10)))))))

(define (most-often-element-by-pairs lst)
  (define (building-pairs lst)
    (define (iter stack tail)
      (cond [(null? tail) stack]
            [(not (ormap (λ(x)(equal? x (car tail))) (map car stack)))
              (iter (cons (cons (car tail) 1) stack) (cdr tail))]
            [else (iter 
              (cons (cons (car tail)(add1 (cdar (filter (λ(x)(equal? (car x)(car tail))) stack))))
              (remove (λ(x)(equal? (car x)(car tail))) stack))(cdr tail))]))
    (iter (list(cons (car lst) 1)) (cdr lst)))
  (let ((list-of-pairs (building-pairs lst)))
    (foldl (λ(pair max)(if (> (cdr pair)(cdr max))
                           pair
                           max)) (car list-of-pairs)(cdr list-of-pairs)))
  )