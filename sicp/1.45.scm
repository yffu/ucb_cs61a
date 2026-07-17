#lang sicp

; 1.45

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (repeated f n)
  (define (iter g k)
    (if (= k 0)
        g
        (iter (lambda (x) (f (g x))) (- k 1))))
  (iter identity n))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (2rt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (2rt2 x)
  (fixed-point (average-damp (lambda(y) (/ x y)))
               1.0))

(2rt2 4)

(define (3rt x)
  (fixed-point (lambda (y) (average y (/ x (expt y 2))))
               1.0))

(define (3rt2 x)
  (fixed-point (average-damp (lambda(y) (/ x (expt y 2))))
               1.0))

(3rt2 27)

(define (4rt x)
  (fixed-point (lambda (y) (average y (/ x (expt y 3))))
               1.0))

(define (4rt2 x)
  (fixed-point ((repeated average-damp 2) (lambda(y) (/ x (expt y 3))))
               1.0))

(4rt2 81)

(define (log2 x)
  (define (iter q n)
    (if (< q 2)
        n
        (iter (quotient q 2) (+ n 1))))
  (iter x 0))

(define (n-rt n x)
  (fixed-point ((repeated average-damp (log2 n)) (lambda(y) (/ x (expt y (- n 1)))))
               1.0))

(log2 32)
(log2 31)
(log2 8)
(log2 4)
(log2 2)
(log2 1)

(display "n-rt")
(newline)

(n-rt 2 9)
(n-rt 3 27)
(n-rt 4 81)
(n-rt 5 243)
(n-rt 6 729)
(n-rt 7 128)
