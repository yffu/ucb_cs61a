#lang sicp

; 1.44

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (square x)
  (* x x))

((smooth square) 2)

(define (identity x) x)

(define (repeated f n)
  (define (iter g k)
    (if (= k 0)
        g
        (iter (lambda (x) (f (g x))) (- k 1))))
  (iter identity n)) ; identity as the base case when n = 0

(define (smooth-nfold f n)
  (repeated smooth n) f)

((smooth-nfold square 4) 2)