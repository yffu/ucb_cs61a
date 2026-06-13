#lang sicp

; 1.17

(define (mult a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (fast-mult a b)
  (cond ((= b 1) a)
        ((= (remainder b 2) 0) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

(mult 2 4)

(fast-mult 3 7)