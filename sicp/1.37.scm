#lang sicp

; 1.37
; a. iterative

(define (cont-frac n d k)
  (define (helper val k)
    (if (= k 0)
        val
        (helper (/ (n k) (+ (d k) val)) (- k 1))))
  (helper 0.0 k))

(define phi-approx (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)))

(define tolerance 0.00005)

(display phi-approx)
(newline)

(define diff (abs (- phi-approx 1.6180339887)))

(display diff)
(newline)

(< diff tolerance) ; within tolerance?
; phi ~ 1.6180 accurate to 4 decimal places means within 0.00005
; .0000215668 12 > within tolerance
; .0000082376 13

; b. recursive
(define (cont-frac-rec n d k)
  (define (helper i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (helper (+ i 1))))))
  (helper 1))

(/ 1 (cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 12))