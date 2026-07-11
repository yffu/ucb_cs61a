#lang sicp

; 1.34

(define (square x) (* x x))

(define (f g) (g 2))

(f square)
; 4

(f (lambda (z) (* z (+ z 1))))
; 6

;(f f) > (f 2) > (2 2)

; error, application not a procedure, as the expression (2 2) is evaluated, 2 cannot be applied as a procedure

; 1.35

