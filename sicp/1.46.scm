#lang sicp

;1.46

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

; previous sqrt implementation for reference
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

(define (iterative-improve check improve)
  (define (try guess)
    (if (check guess)
        guess
        (try (improve guess))))
  try) ; note only procedure is returned, not a procedure call

(define (2rt x)
  (define (improve-sqrt y) ; x define in scope of 2rt, so x is available and doesn't need to be passed as another argument
    (average y (/ x y)))
  (define (check-sqrt y) ; x define in scope of 2rt, so x is available and doesn't need to be passed as another argument
    (< (abs (- (square y) x)) 0.001))
  ((iterative-improve check-sqrt improve-sqrt) 1.0))

(2rt 4)
(2rt 9)
(2rt 16)

; previous fixed-point implementation for reference
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (check-fixpt guess x)
  (< (abs (- guess x)) 0.00001))

(define (fixpt f first-guess)
  (define (check-fixpt y)
    (< (abs (- y (f y))) 0.00001))
;  (define (improve y)
;    (f y)) f can be passed directly as improve
  ((iterative-improve check-fixpt f) first-guess))

(fixpt cos 1.0)