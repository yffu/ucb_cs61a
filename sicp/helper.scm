(provide square)

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))