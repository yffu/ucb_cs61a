#lang sicp

; 2.2
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (avg x1 x2)
  (/ (+ x1 x2) 2))

(define (midpoint-segment segment)
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (make-point (avg (x-point p1) (x-point p2))
                (avg (y-point p1) (y-point p2)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define s1
  (make-segment (make-point 0 0) (make-point 4 4)))

(print-point (midpoint-segment s1))

; 2.3

(define (make-rect s1 s2)
  (cons s1 s2))

(define (rect-s1 rect)
  (car rect))

(define (rect-s2 rect)
  (cdr rect))

(define (make-vector segment)
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (make-point (- (x-point p2) (x-point p1))
                (- (y-point p2) (y-point p1)))))

(define (length segment)
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (sqrt (+ (expt (- (x-point p2) (x-point p1)) 2)
             (expt (- (y-point p2) (y-point p1)) 2)))))

    
(define (area-rect rect)
  (let ((v1 (make-vector (rect-s1 rect)))
        (v2 (make-vector (rect-s2 rect))))
    (abs (- (* (x-point v1) (y-point v2))
            (* (x-point v2) (y-point v1))))))

(define (perimeter-rect rect)
  (let ((l1 (length (rect-s1 rect)))
        (l2 (length (rect-s2 rect))))
    (* (+ l1 l2) 2)))

(define r1
  (make-rect
   (make-segment (make-point 0 0) (make-point 0 2))
   (make-segment (make-point 0 0) (make-point 4 0))))

(area-rect r1)
(perimeter-rect r1)