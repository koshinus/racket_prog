#lang racket
;дан список найти перестановку, чтобы некоторая функция, на этом списке давала бы макс значение
(define (universal lst f)
  (define (in-rec per tail per-res)
    (if (null? tail) 
        (let ((res (f per))) (if (< res (f per-res)) per per-res))
        (foldl (λ (pos res) (in-rec (cons (list-ref tail pos) per) (handle tail pos) res)) per-res (range (length tail)))))
  (define (handle lst pos)
    (append (take lst pos) (drop lst (+ 1 pos))))
  (in-rec null lst lst))
;есть граф заданный списками смежных вершин, проверить, является ли он полным
(define (del lst1 lst2)
  (if (empty? lst2) lst1
      (del (remove (car lst2) lst1) (cdr lst2))))

(define (full? graph)
  (define l (length (car graph)))
  (define g (remove-duplicates (map (λ (x) (del (range 1 (+ l 1)) x)) graph)))
  (if (and (= (length g) 1) (empty? (car g))) "граф полный" "граф неполный"))

;дан граф, номер вершины, нужно найти где находится вершина, находящяся на макс расстоянии
;  1____2                                                                      1____2
;  |    |                                                                      |    |
; 3|____|4____5 -------> '((1 2) (1 3) (2 4) (3 4) (4 5) (3 6))-список ребер  3|____|4____5 -------> '((3) (2 3) (1 4) (1 4 0))
;  |                                                                           |
; 6|                                                                          0|
;(define (too-far lst start)
 ; (define (remove-top gr top)
  ;  (map (λ (ls) (remove top ls)) gr))
  ;(define (next-level list-top graph)
   ; (foldl (λ (top gr) (define current (list-ref gr top)) 
    ;         (foldl (λ (x res) (cons (cons x (car res)) (cdr res)) (remove-top (cdr res) x))) (cons null gr) current) (cons
(define (perm lst)
  (define (handle lst pos) (append (take lst pos) (drop lst (+ 1 pos))))
  (define (in-rec per tail res)
    (if (null? tail) (append per res)
        (foldl (λ (pos res) (in-rec (cons (list-ref tail pos) per) (handle tail pos) res)) '() (range (length tail)))))
  (in-rec '() lst lst))

(define (perm1 lst)
  (define (handle lst pos) (append (take lst pos) (drop lst (+ 1 pos))))
  (define (in-rec per tail res)
    (if (null? tail) (append per res)
        (foldl (λ (pos res) (cons (reverse (in-rec (cons (list-ref tail pos) per) (handle tail pos) res)) res)) '() (range (length tail)))))
  (reverse (in-rec '() lst lst)))
                                                                                                                       