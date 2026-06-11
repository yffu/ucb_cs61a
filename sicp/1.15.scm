#lang sicp

;1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
  (cons angle 0)
  (let ((res (sine (/ angle 3.0))))
    (cons (p (car res)) (+ (cdr res) 1)))))

(sine 12.15)

;a. The procedure p is applied 5 times when the (sine 12.15) is evaluated.

;b. The order of growth in space: is Theta(log_3(a))