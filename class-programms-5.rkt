#lang racket
(define (str2int str)
  (foldl (lambda (x s)
           (+ (* s 10) (- (char->integer x) 48)))
         0
         (string->list str)))
(define (int2str int)
  (define (iter n lst)
    (if (= n 0)
        (if (empty? lst) '(#\0) lst)
        (iter (quotient n 10)
              (cons (integer->char (+ (remainder n 10) 48)) lst))))
  (list->string (iter int '())))

(define (find-str sub str)
  (define n (string-length sub))
  (define rez (memf (λ(i)(equal? sub (substring str i (+ i n))))
                    (build-list (- (string-length str) n) values)))
  (if (equal? rez #f) #f (car rez)))

(define (str2sum str)
  (foldl (lambda (x s)
           (+ s (char->integer x)))
         0
         (string->list str)))

(define (find-hash sub str)
  (define A (str2sum sub))
  (define len1 (string-length sub))
  (define len2 (string-length str))
  (define (find-iter str B n)
    (if (= A B)
        (if (equal? sub (substring str 0 len1)) n
            (if (>= n (- len2 len1)) #f
                (find-iter (substring str 1) (+ (- B
                                                   (char->integer (string-ref str 0)))
                                                (char->integer (string-ref str len1)))
                           (+ n 1))))
        (if (>= n (- len2 len1)) #f
            (find-iter (substring str 1) (+ (- B
                                               (char->integer (string-ref str 0)))
                                            (char->integer (string-ref str len1)))
                       (+ n 1)))))
  (if (> len1 len2) #f
      (find-iter str (str2sum (substring str 0 len1)) 0)))