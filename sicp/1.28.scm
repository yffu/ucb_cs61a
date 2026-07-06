#lang sicp

; 1.28
; pick a random number a < n and raise a to the (n-1)-st power modulo n using the expmod procedure
; (expmod (random n) (- n 1) n)

(define (square n)
  (* n n))

(define (square-root-chk result m)
  (cond ((and
          (not (= result 1))
          (not (= result (- m 1)))
          (= (remainder (square result) m) 1)) 0)
        (else (remainder (square result) m))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-root-chk 
          (expmod base (/ exp 2) m) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1)) ; a ^ (n - 1) and check if == 1
  (try-it (+ 1 (random (- n 1))))) ; random base 1 <= a < n

; Prime numbers
(miller-rabin 199)
(miller-rabin 1999)

; Carmichael numbers
(miller-rabin 561)
(miller-rabin 1105)
(miller-rabin 1729)
(miller-rabin 2465)