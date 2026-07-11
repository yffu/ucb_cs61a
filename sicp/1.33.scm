#lang sicp

; 1.33

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

(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value) (filtered-accumulate combiner filter null-value term (next a) next b))
  ))

(define (sum-square-prime a b)
  (filtered-accumulate + miller-rabin 0 square a inc b))

(sum-square-prime 2 5)
(sum-square-prime 199 230)
(+ (square 199) (square 211) (square 223) (square 227) (square 229))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))



(define (sum-lt-rel-prime n)
  (define (rel-prime i)
    (= 1 (gcd n i)))
  (filtered-accumulate * rel-prime 1 identity 1 inc n))

(sum-lt-rel-prime 7)
(sum-lt-rel-prime 8)
(sum-lt-rel-prime 9)