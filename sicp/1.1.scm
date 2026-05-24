#lang sicp

;1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;1.3
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (ex3 x y z)
  (if (< x y)
      (if (< x z) (sum-of-squares y z) (sum-of-squares y x))
      (if (< y z) (sum-of-squares x z) (sum-of-squares x y))))
(ex3 3 2 3)
(ex3 3 2 3)
(ex3 1 2 3)
(ex3 1 4 3)

;1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 2 -1)
(a-plus-abs-b 2 1)
; if b is positive, return a + b, else return a - b

;1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 0)
; normal-order evaluation would substitute (p) for y in the body of test to (if (= 0 0) 0 (p))). this gets evaluated by if special processing to the consequent, 0
; applicative-order evaluation 1. evaluate the operator to get the operator to be applied 2. evaluate the operands to get the arguments.
; (p) gets evaluated in (test 0 (p)) and go into an infinite loop.
; lisp is applicative-order evaluation, so this would go into an infinite loop.
;(test 0 (p))

;1.6
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs(- (square guess) x)) 0.001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

#|
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
|#

; applicative-order evaluation means that
;(sqrt-iter 1.0 2)
; will retrieve the body of sqrt-iter
;(new-if (good-enough? guess x) guess (sqrt-iter (improve guess x))), replace the formal parameter guess by 1.0 and x by 2
;(new-if (good-enough? 1.0 2) 1.0 (sqrt-iter (improve 1.0 2))), and then evaluate the operands to get the arguments
;evaluating (sqrt-iter (improve 1.0 2)) will recursively call sqrt-iter with the new-if definition and go into an infinite loop

#|
if expression
1. evaluate the <predicate> part of the expression
2. if the <predicate> evaluates to a true value, evaluate the <consequent> and return its value
3. otherwise evaluate the <alternative> and return its value
conditional expression
1. evaluate the predicate <p1>.
2. if <p1> is false, then <p2> is evaluated.
3. this process continues until a predicate is found whose value is true, returns the value of the corresponding consequent expression
|#

;1.7
(define (good-enough2? guess-prev guess)
  (< (abs(/(- guess-prev guess) guess)) 0.001))

(define (sqrt-iter guess x)
(if (good-enough2? guess (improve guess x)) guess
    (sqrt-iter (improve guess x)
               x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt 0.000000000004) ; 4 * 10^(-12)
(sqrt 4000000000000)
; not effective for finding the square roots of very small numbers - since the tolerance in good-enough is absolute the recursive calls end too soon
; inadequate for very large numbers, as (y + x / y) / 2 is a poor approximation of root, and the function converges slowly.
; the precision of the result is improved for very small numbers, but the iterations required to converge for very large numbers does not improve.

;1.8
(define (improve-r3 y x)
  (/ (+ (/ x (square y)) (* 2 y)) 3))
(define (cbrt-iter guess x)
  (if (good-enough2? guess (improve-r3 guess x)) guess
      (cbrt-iter (improve-r3 guess x)
                 x)))
(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 1.0)
(cbrt 8.0)
(cbrt 27.0)