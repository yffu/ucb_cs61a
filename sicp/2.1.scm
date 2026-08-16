#lang sicp

;2.1

; cons: construct
(define x (cons 1 2))

; car : contents of address part of register
(car x)

; cdr: contents of decrement part of register
(cdr x)

(define (make-rat n d)
  (let ((g (gcd n d))
        (n (cond [(and (< n 0) (> d 0)) n]
                 [(and (> n 0) (< d 0)) (* n -1)]
                 [else (abs n)]))
        (d (abs d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(print-rat (make-rat -1 3))

(print-rat (make-rat -1 -3))

(print-rat (make-rat 1 -3))

(print-rat (make-rat 1 3))