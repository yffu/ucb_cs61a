#lang sicp

; 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define pair (cons 1 2))

(car pair)

; (pair (lambda (p q) q))
; ((cons 1 2) (lambda (p q) q))
; ((lambda (m) (m 1 2)) (lambda (p q) q))
; ((lambda (p q) q) 1 2)
; 2

(cdr pair)

