#lang sicp

; this is not to say one should always write programs in the most abstract way possible;
; expert programmers know how to choose the level of abstraction appropriate to their task.

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

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

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(sqrt 2)

; 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 2 3) 1)

(newtons-method (cubic 0 0 -1) 1)

; 1.41

(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x)
  (+ x 1))

((double inc) 2)

((double square) 3)

(((double (double double)) inc) 5)

; (double double) -> (lambda(x) (double (double x))) -> quadruple
; (double (double double)) -> (double quadruple) -> (lambda(x) (quadruple (quadruple x)))
; (double (double double)) inc) -> (inc-x-16 5) -> 21