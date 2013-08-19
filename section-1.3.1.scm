(define (cube x)
  (* x x x))

(define (inc n)
  (+ n 1))

(define (identity x) x)

(define (integral f a b dx)
  (let ((add-dx
	 (lambda (x)
	   (+ x dx))))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))

; ex 1.29
(define (another-integral f a b n)
  (let* ((h (/ (- b a) n))
	 (eval-f
	  (lambda (k)
	    (f (+ a (* k h)))))
	 (term
	  (lambda (k)
	    (cond ((or (= k 0)
		       (= k n))
		   (eval-f k))
		  ((odd? k)
		   (* 4 (eval-f k)))
		  (else
		   (* 2 (eval-f k)))))))
    (* (/ h 3.0)
       (sum term 0 inc n))))

; ex 1.31
(define (factorial n)
  (product identity 1 inc n))

(define (compute-pi n)
  (let ((term
	 (lambda (k)
	   (if (odd? k)
	       (/ (+ k 1) (+ k 2))
	       (/ (+ k 2) (+ k 1))))))
    (* 4.0 (product term 1 inc n))))

(define (iter-compute-pi n)
  (let ((term
	 (lambda (k)
	   (if (odd? k)
	       (/ (+ k 1) (+ k 2))
	       (/ (+ k 2) (+ k 1))))))
    (* 4.0 (iter-product term 1 inc n))))

; ex 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (iter-accumulate combiner null-value term a next b)
  (letrec ((iter
	    (lambda (x result)
	      (if (> x b)
		  result
		  (iter (next x) (combiner (term x) result))))))
    (iter a null-value)))

(define (iter-sum term a next b)
  (iter-accumulate + 0 term a next b))

(define (iter-product term a next b)
  (iter-accumulate * 1 term a next b))

; ex 1.3.3
(define (filtered-accumulate combiner null-value term a next b filter)
  (letrec ((iter
	    (lambda (x result)
	      (if (> x b)
		  result
		  (iter (next x)
			(if (filter x)
			    (combiner (term x) result)
			    result))))))
    (iter a null-value)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

