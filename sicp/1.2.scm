#lang sicp
;1.9
#|
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
recursive process, as deferred operation of inc for count of a is expanded out
|#

#|
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
iterative process, as state is captured by (dec a) and (inc b) and their sum. once 0 is zero, b contains all of the sum
|#

;1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;2^10 or 1024

(A 2 4)
;2^16 or 65536

(A 3 3)
;2^16 or 65536

(define (f n) (A 0 n))
; f(n) = 2*n
(f 8)

(define (g n) (A 1 n))
;f(n) = 2^n
(g 4)

(define (h n) (A 2 n))
;g(n) = 2^(2^(2^(2^(n-4))))
(h 4)

(define (k n) (* 5 n n))
;k(n) = 5*n^2

;1.11
(define (ex11 n)
  (if (< n 3)
      n
      (+ (ex11 (- n 1))
         (* 2 (ex11 (- n 2)))
         (* 3 (ex11 (- n 3))))))

(ex11 6)

(define (ex11b n)
  (ex11-iter 2 1 0 n))

(define (ex11-iter n1 n2 n3 n)
  (if (< n 3)
      n1
      (ex11-iter (+ n1 (* 2 n2) (* 3 n3)) n1 n2 (- n 1))))

(ex11b 3)
(ex11b 4)
(ex11b 5)
(ex11b 6)

;1.12
(define (pascal row col)
  (cond ((< row 1) 0)
        ((< col 1) 0)
        ((= col 1) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))
  ))

(pascal 1 1)
(pascal 2 1)
(pascal 2 2)
(pascal 3 2)
(pascal 5 3)

;1.14
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
(cond ((= amount 0) 1)
((or (< amount 0) (= kinds-of-coins 0)) 0)
(else (+ (cc amount
(- kinds-of-coins 1))
(cc (- amount
(first-denomination
kinds-of-coins))
kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
(cond ((= kinds-of-coins 1) 1)
((= kinds-of-coins 2) 5)
((= kinds-of-coins 3) 10)
((= kinds-of-coins 4) 25)
((= kinds-of-coins 5) 50)))

