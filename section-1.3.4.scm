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

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; ex 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(define (inc n)
  (+ n 1))

; ex 1.41
(define (double f)
  (lambda (x) (f (f x))))

; ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; ex 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

; ex 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))

(define (nth-smooth f n)
  ((repeated smooth n) f))

; ex 1.45
(define (nth-root-helper x n)
  (lambda (y) (/ x (expt y (- n 1)))))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (sqrt n)))
		(nth-root-helper x n))
	       1.0))

; ex 1.46
(define (iterative-improve good-enough? improve)
  (letrec ((try (lambda (guess)
		  (if (good-enough? guess)
		      guess
		      (try (improve guess))))))
    try))

(define (another-fixed-point f guess)
  (let* ((good-enough?
	  (lambda (guess)
	    (< (abs (- (f guess) guess)) tolerance)))
	 (improve (lambda (guess) (f guess))))
    ((iterative-improve good-enough? improve) guess)))

(define (another-sqrt x)
  (let* ((improve (lambda (guess)
		    (average guess (/ x guess))))
	 (good-enough?
	  (lambda (guess)
	    (< (abs (- (improve guess) guess)) tolerance))))
    ((iterative-improve good-enough? improve) 1.0)))
