; ex 1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (letrec ((try (lambda (guess)
		  (let ((close-enough?
			 (lambda (v1 v2)
			   (< (abs (- v1 v2)) tolerance)))
			(next (f guess)))
		    (if (close-enough? guess next)
			next
			(try next))))))
    (try first-guess)))

(define (fixed-point-detail f first-guess)
  (letrec ((try (lambda (guess)
		  (let ((close-enough?
			 (lambda (v1 v2)
			   (< (abs (- v1 v2)) tolerance)))
			(next (f guess)))
		    (begin (newline)
			   (display "***")
			   (display next)
			   (display "***")
			   (if (close-enough? guess next)
			       next
			       (try next)))))))
    (try first-guess)))

; ex 1.37
(define (cont-frac n d k)
  (letrec ((cont-frac-rec
	    (lambda (cur)
	      (if (= cur k)
		  (/ (n cur) (d cur))
		  (/ (n cur)
		     (+ (d cur)
			(cont-frac-rec (+ cur 1))))))))
    (cont-frac-rec 1)))

(define (cont-frac-iter n d k a)
  (if (= k 1)
      (/ (n k) (+ (d k) a))
      (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) a)))))

(define (test-cont-frac-accuracy f k)
  (let ((n-and-d (lambda (i) 1.0))
	(tolerance 0.0001)
	(golden-radio (/ (- (sqrt 5) 1) 2)))
    (< (abs (- (f n-and-d n-and-d k) golden-radio)) tolerance)))

; ex 1.38
(define (another-euler-s-number-calculate-d i)
  (let ((n (+ i 1)))
    (if (= (remainder n 3) 0)
	(* (/ n 3) 2)
	1)))

(define (another-euler-s-number-calculate k)
  (+ (cont-frac (lambda (i) 1.0) another-euler-s-number-calculate-d k) 2))

; ex 1.39
(define (tan-cf-d k)
  (- (* 2 k) 1))

(define (tan-cf x k)
  (let ((n (lambda (i)
	     (if (= i 1)
		 x
		 (- 0 (* x x))))))
    (cont-frac n tan-cf-d k)))
