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

(define (count-word str)
  (define (iter last str k)
    (if (equal? str "")
        k
        (iter (string-ref str 0)(substring str 1)(if (and 
                                    (equal? last #\space) 
                                    (not (equal? #\space (string-ref str 0))))
                                   (+ k 1)
                                   k
                                   )
              )
        )
    )
  (iter #\space str 0)
  )