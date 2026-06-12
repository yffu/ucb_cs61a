#lang sicp

;1.16

(define (even? n)
(= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (fast-expr b n)
  (fast-expr-iter b n 1))

(define (fast-expr-iter b n a)
  (cond ((= n 1) (* a b))
        ((even? n) (fast-expr-iter (square b) (/ n 2) a))
        (else (fast-expr-iter b (- n 1) (* a b)))))

(fast-expr 2 4)
(fast-expr 2 5)
(fast-expr 3 5)