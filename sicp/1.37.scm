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

; 1.38
; Ni are all 1 and Di are
; 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1,  8,
; cycle length 3, (k + 1) remainder 3 == 0
; (k + 1) quotient 3 == 1

(define (series-d i)
  (let ((i-shift (+ i 1)))
    (if (= (remainder i-shift 3) 0)
        (* (quotient i-shift 3) 2)
        1)))

(series-d 10)
(series-d 11)

(define (e-cf n)
  (+ 2
     (cont-frac (lambda (i) 1.0)
           series-d n)))

(e-cf 12)

; 1.39

(define (cont-frac-n n d k)
  (define (helper val k)
    (if (= k 0)
        val
        (helper (/ (n k) (- (d k) val)) (- k 1))))
  (helper 0.0 k))

(define (square x)
  (* x x))

(define (tan-cf x k)
  (cont-frac-n
   (lambda (i) (if (= i 1) x (square x)))
   (lambda (i) (- (* 2 i) 1)) k))

(tan (/ 3.1415926535 4))

(tan-cf (/ 3.1415926535 4) 23)

