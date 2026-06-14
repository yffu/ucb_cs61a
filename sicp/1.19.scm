#lang sicp

(define (even? n)
(= (remainder n 2) 0))

(define (square n)
  (* n n))

; 1.19

(define (fib n )
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(fib 5)

; a <- a + b
; b <- a

(set! fib (lambda (n)
  (fib-iter 1 0 n)))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(fib 5)

; T_pq
; a <- bq + aq + ap
; b <- bp + aq

; a' <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
; b' <- (bp + aq)p + (bq + aq + ap)q

; a' <- bpq + aqq + bqq + aqq + apq + bpq + apq + app
; = b(qq+2pq) + a(2qq+2pq+pp)
; = b * q * (q+2p) + a * (2q + p)(q + p)

; b' <- bpp + apq + bqq + aqq + apq
; = bp^2 + 2apq + (a + b)q^2
; = b * (p^2 + q^2) + a * (2pq + q^2)

; q' = (2pq + q^2)
; q' + p' = (2 * q^2 + 2pq + p^2)
; p' = 2q^2 + 2pq + p^2 - (2pq + q^2) = q^2 + p^2

(set! fib (lambda (n)
  (fib-fast-iter 1 0 0 1 n)))
(define (fib-fast-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-fast-iter a
                        b
                        (+ (* p p) (* q q))
                        (+ (* 2 p q) (* q q))
                        (/ count 2)))
        (else (fib-fast-iter (+ (* b q) (* a q) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- count 1)))))

(fib 5)
(fib 12)