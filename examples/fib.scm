(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(displayln "Computing...")
(let ((ans (fib 35)))
  (display "(fib 35) => ")
  (display (fib 35))
  (newline))
