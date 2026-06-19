#lang sicp

;(load "C:/Users/yuanf/OneDrive/0_course/ucb_cs61a/sicp/helper.scm")
;(load "helper.scm")
; 2 numbers are congurent modulo n if they both have the same remainder when divided by n
; test a^n % n == a, if false n is not prime. if true n is likely prime, then try another random a and test

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(expmod 2 8 4)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n ) a))
  (try-it (+ 1 (random (- n 1)))))

(fermat-test 127)

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 127 5)

; 1.21

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; check the primality of consecutive odd integers in a specified range.

(define (search-for-primes n i)
  (cond ((= i 0) (newline))
        ((prime? (+ n 1)) (timed-prime-test (+ n 1))
                          (search-for-primes (+ n 2) (- i 1)))
        (else (search-for-primes (+ n 2) i))))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)

; sqrt(10) = 3.16
; 1000: 8, 3, 4
; 10000: 8, 8, 7 ~ 3x the time of the 1000
; 100000: 22, 25, 19 ~ 3x the time of the 10000
; the increases follow the order of growth but are not as large, perhaps due to constant cost for overhead. this is more noticeable in the first invocation where the runtime is 8

; 1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(set! find-divisor (lambda(n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor))))))

(set! search-for-primes (lambda(n i)
  (cond ((= i 0) (newline))
        ((prime? (+ n 1)) (timed-prime-test (+ n 1))
                          (search-for-primes (+ n 2) (- i 1)))
        (else (search-for-primes (+ n 2) i)))))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)

; 1000: 2 2 2 ratio (2 + 2 + 2) / (8 + 3 + 4) = 0.4 
; 10000: 6 6 5 ratio (6 + 6 + 5) / (8 + 8 + 7) = 0.7391 
; 100000: 12 16 15 (12 + 16 + 15) / (22 + 25 + 19) = 0.6515 
; ratio is not 2 but generally less, as there is overhead cost that lowers the ratio

; 1.24
(display 1.24)
(newline)

(set! timed-prime-test (lambda (n)
  (newline)
  (display n)
  (start-prime-test n (runtime))))
(set! start-prime-test (lambda (n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (runtime) start-time)))))

(set! search-for-primes (lambda(n i)
  (cond ((= i 0) (newline))
        ((prime? (+ n 1)) (timed-prime-test (+ n 1))
                          (search-for-primes (+ n 2) (- i 1)))
        (else (search-for-primes (+ n 2) i)))))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)

; 1000: 25 19 16
; 10000: 21 18 31
; 100000: 39 20 42
; 1000000: 27 25 27

; due to the fermat method choosing a random number the runtime does not always trend up with increased n

; 1.25
; expmod and fast-expt are similar in that they both reduce the input by half, resulting in Theta(log(n)) steps
; but expmod is more efficient as it carries out the remainder operation after each recursive call and reduces the size of operands
; the fast-expt version forces the raminder operation until the very end and force fast-expt to compute very large intermediate numbers.
(define (fast-expt b n )
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder
;          (square (expmod base (/ exp 2) m))
;          m))
;        (else
;         (remainder
;          (* base (expmod base (- exp 1) m))
;          m))))

; 1.26
; due to applicative-order processing in scheme, all the arguments to Scheme procedures are avaluaed when the procedure is applied, namely for the square function.
; this causes only one version of expmod to be evaluated before being squared. If the version with multiplication of 2 expmod operand is used, then both expmod operands are evaluated resulting in duplicate work.
; each level has 2^n nodes at level k, for Theta(n) growth rate.

; note: comment out this definition of expmod, or the 1.27 solution won't work.
;(set! expmod (lambda (base exp m)
;               (cond ((= exp 0) 1)
;                     (even? exp)
;                     (remainder (* (expmod base (/ exp 2) m)
;                                   (expmod base (/ exp 2) m)
;                                   m))
;                     (else (remainder (* base
;                                         (expmod base (- exp 1) m))
;                                      m)))))

; 1.27
(define (sure-prime? n)
  (define (try-it n a)
    (cond ((= a 0) true)
          ((= (expmod a n n) a) (try-it n (- a 1)))
          (else false)))
  (try-it n (- n 1)))

; Prime numbers
(sure-prime? 199)
(sure-prime? 1999)

; Carmichael numbers
(sure-prime? 561)
(sure-prime? 1105)
(sure-prime? 1729)
(sure-prime? 2465)