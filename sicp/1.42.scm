#lang sicp

; 1.42

(define (compose f g)
  (lambda(x) (f (g x))))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

((compose square inc) 6)

; 1.43

;(define (repeated f n)
;  (define (iter g k)
;    (if (= k 1)
;        g
;        (iter (f g) (- n 1))))
;  (lambda (x) ((iter f n) x)))

; above does not work, as (iter (f g) (- n 1)) tries to pass the procedure g into f when a value is expected

(define (identity x) x)

(define (repeated2 f n)
  (define (iter g k)
    (if (= k 0)
        g
        (iter (lambda (x) (f (g x))) (- k 1))))
  (iter identity n)) ; identity as the base case when n = 0

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)
((repeated square 1) 5)
((repeated2 square 2) 5)
((repeated2 square 1) 5)
((repeated2 square 2) 3)
((repeated2 square 0) 9)