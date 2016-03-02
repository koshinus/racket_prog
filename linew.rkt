#lang racket
(define (lines rows cols len)
  (+ 
   (quotient (* (quotient rows len) cols) len)
   (quotient (* (- rows (quotient rows len)) cols) len)))
