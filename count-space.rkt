#lang racket
(define (count-space str)
  (define (iter str k)
    (if (equal? str "")
        k
        (iter (substring str 1)(if (equal? (string-ref str 0) #\space)
                                   (+ k 1)
                                   k
                                   )
              )
        )
    )
  (iter str 0)
  )