#lang sicp

; 1.32

(define (identity x) x)
(define (inc n) (+ n 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))
  ))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 identity a inc b))

(define (sum term a next b)
  (accumulate + 0 identity a inc b))

(define (product-iter term a next b)
  (accumulate-iter * 1 identity a inc b))

(define (factorial n)
  (product-iter identity 1 inc n))

(factorial 4)