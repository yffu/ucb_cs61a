#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; (gcd 206 40)
; (remainder 206 40) > (40 6) > (6 4) > (4 2) > (gcd 2 0) > end
; normal-order evaluation performs 4 remainder operations, terminating on (gcd 2 0) where b = 0

; applicative-order evaluation
; (gcd 206 40) >
(if (= 40 0) 206 (gcd 40 (remainder 206 40))); 0 call

(if (= (remainder 206 40) 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))) ; 1 call (if clause)

(if (= (remainder 40 (remainder 206 40)) 0) (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ; 2 call (if clause) 

(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))) ; 4 call (if clause)

; if clause for (= b 0)
; (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) ; 7 call (if clause)
; a has another 4 calls for (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; 1 + 2 + 4 + 7 + 4 = 18 calls

; remainder is called 10 times, once for the condition of the if clause, and another for the alternative expression
; answer is 18 remainder calls for applicative-order evaluation