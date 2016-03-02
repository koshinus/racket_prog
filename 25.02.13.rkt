#lang racket
(define (f lst)
  (define (help lst1 best cur maxi n)
    (if (empty? lst1) (if (> maxi n) best cur)
        (if (> (* (car cur) (car lst1)) 0)
            (help (cdr lst1) best (cons (car lst1) cur) maxi (+ n 1))
            (if (> maxi n) 
                (help (cdr lst1) best (cons (car lst) '()) maxi 1)
                (help (cdr lst1) cur (cons (car lst) '()) n 1)))))
  (help (cdr lst) '() (cons (car lst) '()) 0 1))

;дано число нужно найти все простые числа до него (решето Эратосфена)
;insert-k k mn
;remove-k k mn
;ok? k mn
;build-mn
;empty-mn?

(define (sieve n)
  (define (find k mn m)
    (if (ok? m mn) m 
        (find-k mn (+ m 1))))
  (define (remove-kr mn x)
    (define (help mn k)
      (if (> k n) mn
          (help (remove-k k mn) (+ k x)))))
  (define (main set primes s)
    (if (empty-mn? set) primes (let ((cur-pr (find-k sets))) (main (remove-kr set cur-pr) (cons cur-pr primes) (+ s 1)))))
  (define (build-mn-k set k)
    (if (= k 1) set
        (build-mn-k (insert-k k set) (sub1 k))))
  (main (build-mn-k (build-mn) n) null 2))