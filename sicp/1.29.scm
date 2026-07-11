#lang sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-simpson term a next b)
  (if (>= a b)
      0
      (+ (term a)
         (sum-simpson term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

; 1.29 have to use sum-simpson with (>= a b) instead of (> a b) to address an off by one bug. integral of x^3 from 0 to 1 is 0.25 exact
(define (int-simpson f a b n)
  (define h (/ (- b a) n))
  (define (add-dx x)
    (+ x (* 2 h)))
  (define (term-even a)
    (+ (f a) (f (add-dx a))))
  (* (/ h 3.0)
     (+ (sum-simpson term-even a add-dx b)
        (* 4 (sum-simpson f (+ a h) add-dx b)))))

(int-simpson cube 0 1 100)
(int-simpson cube 0 1 1000)
