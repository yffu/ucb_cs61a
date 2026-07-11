#lang sicp

; 1.31

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec term (next a) next b))))

(define (identity x) x)
(define (inc n) (+ n 1))

(define (factorial n)
  (product identity 1 inc n))

(factorial 4)

(define (pi-prod n)
  (define (pi-term x)
    (define numer (* 2 (quotient (+ x 3) 2)))
    (define denom (+ (* 2 (quotient (+ x 2) 2)) 1))
    (/ numer denom))
  (* (product pi-term 0 inc n) 4.0))

(define (pi-prod-rec n)
  (define (pi-term x)
    (define numer (* 2 (quotient (+ x 3) 2)))
    (define denom (+ (* 2 (quotient (+ x 2) 2)) 1))
    (/ numer denom))
  (* (product-rec pi-term 0 inc n) 4.0))

(pi-prod 100)
(pi-prod 1000)

(pi-prod-rec 100)
(pi-prod-rec 1000)