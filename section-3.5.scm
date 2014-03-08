(define the-empty-stream '())

(define stream-null? null?)

(define-syntax cons-stream
  (syntax-rules ()
    ((_ first second) (cons first (delay second)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? #t)
		 result)
	  result))))

; ex 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules ()
    ((_ first second) (cons first (delay second)))))

(define stream-null? null?)

;
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
	       (cons-stream 1
			    (add-streams (stream-cdr fibs)
					 fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 2))))

(define (prime? n)
  (letrec ((iter
	    (lambda (ps)
	      (cond ((> (square (stream-car ps)) n) #t)
		    ((divisible? n (stream-car ps)) #f)
		    (else (iter (stream-cdr ps)))))))
    (iter primes)))

; ex 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1
	       (mul-streams (integers-starting-from 2)
			    factorials)))

; ex 3.55
(define (partial-sums stream)
  (cons-stream (stream-car stream)
	       (add-streams (stream-cdr stream)
			    (partial-sums stream))))

; ex 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car
			       (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))

(define S (cons-stream 1
		       (merge (scale-stream S 2)
			      (merge (scale-stream S 3)
				     (scale-stream S 5)))))

; ex 3.58
(define (expand num den radix)
  (cons-stream
   (quotinet (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; ex 3.59
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series stream)
  (div-streams stream integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream
		  (integrate-series sine-series)
		  -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; ex 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
		  (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s2)
					  (stream-car s1))
			    (mul-series (stream-cdr s1) s2))))

; ex 3.61
(define (invert-unit-series series)
  (cons-stream 1
	       (scale-stream (mul-series (stream-cdr series)
					 (invert-unit-series series))
			     -1)))

; ex 3.62
(define (div-series num denom)
  (let ((denom-const (stream-car denom)))
    (if (= denom-const 0)
	(error "denom constant term is zero" 'div-series)
	(mul-series
	 num
	 (scale-stream
	  (invert-unit-series
	   (scale-stream denom (/ 1 denom-const)))
	  denom-const)))))

(define tangent-series
  (div-series sine-series cosine-series))
