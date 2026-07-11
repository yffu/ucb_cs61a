#lang sicp

; 1.31

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)
(define (inc n) (+ n 1))

(define (factorial n)
  (product identity 1 inc n))

(factorial 4)

(define (pi-prod a b)
  (define (pi-term x)
    (define numer (* 2 (quotient (+ x 3.0) 2)))
    (define denom (+ (* 2 (quotient (+ x 2.0) 2)) 1))
;    (display numer)
;    (display "/")
;    (display denom)
;    (newline)
    (/ numer denom))
  (* (product pi-term a inc b) 4))

(pi-prod 0 100)
(pi-prod 0 1000)