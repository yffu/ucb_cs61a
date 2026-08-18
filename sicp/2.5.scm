#lang sicp

; 2.5

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define pair (cons 2 3))

; tail recursion for O(1) space

(define (car x)
  (define (iter x n)
    (if (= (modulo x 2) 0)
        (iter (/ x 2) (+ n 1))
        n))
  (iter x 0))

(define (cdr x)
  (define (iter x n)
    (if (= (modulo x 3) 0)
        (iter (/ x 3) (+ n 1))
        n))
  (iter x 0))

(car pair)

(cdr pair)



