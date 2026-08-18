#lang sicp

; 2.6

(define zero (lambda (f) (lambda (x) x)))

; zero is a function that takes f and returns a function that takes x, applying f zero times and returns x unchanged

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)

; use substitution to evaluate (add-1 zero)
; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f)
  (lambda (x)
    (f (((lambda (f1) (lambda (x1) x1)) f) x))))

; f/f1 passed into function, yields identity function 
(lambda (f)
  (lambda (x)
    (f ((lambda (x1) x1) x))))

; x passed into function
(lambda (f)
  (lambda (x)
    (f x)))

(define one
  (lambda (f) (lambda (x) (f x))))

(add-1 one)

(lambda (f) (lambda (x) (f ((one f) x))))

(lambda (f)
  (lambda (x)
    (f (((lambda (f1) (lambda (x1) (f1 x1))) f) x))))

(lambda (f)
  (lambda (x)
    (f (lambda (x1) (f x1)) x)))

(lambda (f)
  (lambda (x)
    (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

;; Direct addition function for Church numerals
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))
