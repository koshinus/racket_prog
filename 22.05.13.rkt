#lang racket
;3.76
(define (int? st1 st2)
  (define a (+ (* (car (car st1)) (car st2)) (* (cdr (car st1)) (cadr st2)) (caddr st2)))
  (define b (+ (* (car (cadr st1)) (car st2)) (* (cdr (cadr st1)) (cadr st2)) (caddr st2)))
  (if (<= (* a b) 0) #t #f))

(define (intersection line pol)
  (define (help line pol p)
    (if (empty? (cdr pol)) (int? (list (car pol) p) line)
        (if (int? (list (car pol) (cadr pol)) line) #t (help line (cdr pol) p))))
  (help line pol (car pol)))

;3.71
(define (self-int? pol)
  (define a (append pol (list (car pol))))
  (define (help pol1 pol2 pol3 pol4)
    (if (exist-dup pol4) #t
    (if (empty? (cdr pol1)) #f
        (if (empty? (cdr pol2)) (help (cdr pol1) pol3 pol3 pol4)
            (if (check (list (car pol1) (cadr pol1)) (list (car pol2) (cadr pol2))) (help pol1 (cdr pol2) pol3 pol4)
                (if (int-seg? (list (car pol1) (cadr pol1)) (list (car pol2) (cadr pol2))) #t (help pol1 (cdr pol2) pol3 pol4)))))))
  (help a a a pol))

(define (exist-dup lst)
  (if (equal? lst (remove-duplicates lst)) #f #t))

(define (member? m lst)
  (if (empty? lst) #f
      (if (equal? m (car lst)) #t
          (member? m (cdr lst)))))

(define (line-factor line)
  (define A (- (cdr (cadr line)) (cdr (car line))))
  (define B (* -1 (- (car (cadr line)) (car (car line)))))
  (define C (- (* -1 (cdr (car line)) B) (* (car (car line)) A)))
  (if (and (not (= 0 (gcd B C))) (= (gcd A B) (gcd B C))) (list (/ A (gcd B C)) (/ B (gcd B C)) (/ C (gcd B C))) (list A B C)))

(define (check seg1 seg2)
  (if (or (equal? seg1 seg2) (or (member? (car seg1) seg2) (member? (cadr seg1) seg2))) #t #f))

(define (int-seg? seg1 seg2)
  (if (and (int? seg1 (line-factor seg2)) (int? seg2 (line-factor seg1))) #t #f))
;3.72
(define (reg-pol n)
  (define (help n i pol)
    (if (= i n) pol
        (help n (+ i 1) (cons (cons (cos (/ (* i 2 pi) n)) (sin (/ (* i 2 pi) n))) pol))))
  (help n 0 '()))
;;;;;;;;;;;;;;;;;;;;;;;
(define (self-int1? pol)
  (define (help pol lines)
    (if (empty? lines) #f
        (if (intersection (caddr (car lines)) (remove (car (car lines)) (remove (car (car lines)) (remove (cadr (car lines)) pol)))) #t
            (help pol (cdr lines)))))
  (help (append pol (list (car pol))) (line-pol pol)))

(define (line-pol pol)
  (define (help pol lst)
    (if (empty? (cdr pol)) (reverse lst)
        (help (cdr pol) (cons (list (car pol) (cadr pol) (line-factor (list (car pol) (cadr pol)))) lst))))
    (help (append pol (list (car pol))) '()))