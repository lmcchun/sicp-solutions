; ex 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (next-divisor test-divisor) ; ex 1.23
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (and (> n 1) (= n (smallest-divisor n))))

; ex 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (if (<= start end)
      (if (= start 1)
	  (search-for-primes 2 end)
	  (let ((start-time (runtime)))
	    (if (prime? start)
		(let ((end-time (runtime)))
		  (begin (display start)
			 (report-prime (- end-time start-time))
			 (newline)))
		(search-for-primes (+ start 1) end))))))

; ex 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m)) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (let ((try-it
	 (lambda (a)
	   (= (expmod a n n) a))))
    (try-it (+ 1 (random (- n 1))))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (fast-search-for-primes start end)
  (if (<= start end)
      (if (= start 1)
	  (fast-search-for-primes 2 end)
	  (let ((start-time (runtime)))
	    (if (fast-prime? start 10)
		(let ((end-time (runtime)))
		  (begin (display start)
			 (report-prime (- end-time start-time))
			 (newline)))
		(fast-search-for-primes (+ start 1) end))))))

; ex 1.27
(define (full-fermat-test n)
  (letrec ((auc-test
	 (lambda (a)
	   (cond ((= a 1) true)
		 ((not (= (expmod a n n) a)) false)
		 (else (auc-test (- a 1)))))))
    (auc-test (- n 1))))

(define (find-carmichaels start end)
  (do ((i start (+ i 1)))
      ((= i end))
    (if (and (odd? i)
	     (full-fermat-test i)
	     (not (prime? i)))
	(begin (display i) (newline)))))

; ex 1.28
(define (another-expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (let* ((candidate (expmod base (/ exp 2) m))
		(root (remainder (square candidate) m)))
	   (if (and (> candidate 1)
		    (< candidate (- m 1))
		    (= root 1))
	       0
	       root)))
	(else
	 (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (let ((try-it
	 (lambda (a)
	   (= (another-expmod a (- n 1) n) 1))))
    (try-it (+ 1 (random (- n 1))))))
