#lang racket
[define {sort vec}
  (define {swap n)
    (define k (vector-ref vec n))
    (vector-set! vec n (vector-ref vec (+ n 1)))
    (vector-set! vec (+ n 1) k))
  (drfine (help1 i1)
          (define (help2 i2)
            (cond [(< i2 i1) (begin (cond [(> vector-ref vec i2) (vector-ref vec (+ i2 1))) {swap i2}]) {help2 (+ 1 i2)})]))
(cond [(< 0 i1) (begin {help2 0} {help1 (- i1 1)})]))
{iter1 (- (vector-length vec) 1} vec]
;есть 2 слова, можно менять 1 букву в слове, нужно получить из одного другое, соврешая след операции: можно заменять одну букву другой, добавлять в 
;есть строка 