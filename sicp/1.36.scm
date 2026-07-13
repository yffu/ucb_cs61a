#lang sicp

; 1.36

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display "without avg damping") ; ~ 29 steps
(newline)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(display "with avg damping") ; ~ 8 steps
(newline)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)