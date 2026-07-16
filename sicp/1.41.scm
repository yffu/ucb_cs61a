#lang sicp

; 1.41

(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x)
  (+ x 1))

((double inc) 2)

((double square) 3)

(((double (double double)) inc) 5)

; (double double) -> (lambda(x) (double (double x))) -> quadruple
; (double (double double)) -> (double quadruple) -> (lambda(x) (quadruple (quadruple x)))
; (double (double double)) inc) -> (inc-x-16 5) -> 21