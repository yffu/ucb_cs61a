#lang sicp

; 2.2
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (avg x1 x2)
  (/ (+ x1 x2) 2))

(define (midpoint-segment segment)
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (make-point (avg (x-point p1) (x-point p2))
                (avg (y-point p1) (y-point p2)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define s1
  (make-segment (make-point 0 0) (make-point 4 4)))

(print-point (midpoint-segment s1))