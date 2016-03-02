#lang racket
;транспонирует матрицу
(define (transpon mat)
  (define (iter tail result)
    (if (null? (car tail))
        (reverse result)
        (iter (map cdr tail) (cons (map car tail) result))))
  (iter mat '()))
;суммирование матриц
(define (matrix-sum mat1 mat2)
  (map (λ(x y)(map + x y)) mat1 mat2))
;умножает матрицу на матрицу
(define (matrix-multiply mat1 mat2)
  (define transp-mat2 (transpon mat2))
  (define (iter tail1 tail2 row result)
    (cond [(null? tail1) (reverse result)]
          [(null? tail2) (iter (cdr tail1) transp-mat2 '() (cons (reverse row) result))]
          [else (iter tail1 (cdr tail2) 
                      (cons (apply + (map * (car tail1)(car tail2))) row) result)]))
  (iter mat1 transp-mat2 '() '()))
;скалярное произведение векторов
(define (vector-multiply vec1 vec2)
  (apply +(map * vec1 vec2)))
(define (matrix-multiply2 mat1 mat2)
  (define transp-mat2 (transpon mat2))
  (if (= (length (car mat1)) (length mat2))
      (map (λ(row-mat1)
             (map (λ(row-mat2)
                    (vector-multiply row-mat1 row-mat2))
                  transp-mat2))mat1)
      (error "Different sizes of matrix")))
;умножение матрицы на скаляр
(define (mat-scalar-multyply scalar mat)
  (map (λ(row)(map (λ(element)(* element scalar)) row)) mat))
;след матрицы
(define (tr mat)
  (define (iter tail result)
    (if (null? tail)
        result
        (iter (map cdr (cdr tail))(+ result (caar tail)))))
  (iter mat 0))
(define (tr2 mat)
  (foldl (λ(row pos result)(+ result (list-ref row pos)))
         0 mat (range (length mat))))
;определитель матрицы
;(define (det mat)
;;; нужен алгорим разбиения матрицы на нижне- 
;;; и верхнедиагональную матрицы
; (define L ())
; (define U ())

;;; у таких матриц определитель - произве-
;;; дение элементов главной диагонали
;  (+ (tr L)(tr U)))