#lang sicp

; 1.17

(define (mult a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (fast-mult a b)
  (cond ((= b 1) a)
        ((= (remainder b 2) 0) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

(mult 2 4)

(fast-mult 3 7)

; 1.18
; 2 * 6 > 4 * 3 > 4 * 1 + 4 * 2 > 4 * 2 + n, where n is 1 * 4
(define (fast-mult-iter a b n)
  (cond ((= b 1) (+ a n))
        ((= (remainder b 2) 0) (fast-mult-iter (double a) (halve b) n))
        (else (fast-mult-iter a (- b 1) (+ n a)))))

(fast-mult-iter 3 7 0)
(fast-mult-iter 1001 1001 0)

                                            
  