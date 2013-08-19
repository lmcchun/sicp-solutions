; ex 1.16
(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (halve n)
  (/ n 2))

(define (another-fast b n)
  (another-fast-expr b n 1))

(define (another-fast-expr b n a)
  (cond ((= n 0) a)
	((even? n) (another-fast-expr (square b) (halve n) a))
	(else (another-fast-expr b (- n 1) (* a b)))))

; ex 1.1.7
(define (double n)
  (+ n n))

(define (fast-multi a b)
  (cond ((= b 1) a)
	((even? b) (double (fast-multi a (halve b))))
	(else (+ a (fast-multi a (- b 1))))))

; ex 1.1.8
(define (another-fast-multi a b)
  (another-fast-multi-iter a b 0))

(define (another-fast-multi-iter a b c)
  (cond ((= b 1) (+ a c))
	((even? b) (another-fast-multi-iter (double a) (halve b) c))
	(else (another-fast-multi-iter a (- b 1) (+ c a)))))

; ex 1.1.9
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (* p p) (* q q))
		   (+ (* 2 p q) (* q q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))
