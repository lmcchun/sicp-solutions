; ex 4.17
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (let ((val
		 (scan (frame-variables frame)
		       (frame-values frame))))
	    (if (eq? val '*unassigned*)
		(error "Using an unassigned variable" var)
		val)))))
  (env-loop env))

(define (get-defines-and-non-defines sexp)
  (letrec ((iter
	    (lambda (sexp defines non-defines)
	      (if (null? sexp)
		  (cons defines non-defines)
		  (let ((head (car sexp))
			(rest (cdr sexp)))
		    (if (definition? head)
			(iter rest (cons head defines) non-defines)
			(iter rest defines (cons head non-defines))))))))
    (iter sexp '() '())))

(define (reverse-list list)
  (letrec ((iter
	    (lambda (list acc)
	      (if (null? list)
		  acc
		  (iter (cdr list)
			(cons (car list) acc))))))
    (iter list '())))

(define (scan-out-defines original-body)
  (let ((defines-and-non-defines
	  (get-defines-and-non-defines original-body)))
    (let ((defines (car defines-and-non-defines))
	  (non-defines (cdr defines-and-non-defines)))
      (let ((make-body
	     (lambda (defines initforms body)
	       (if (null? defines)
		   (make-let initforms body)
		   (let ((head (car defines)))
		     (let ((name (definition-variable head))
			   (value (definition-value head)))
		       (make-body (cdr defines)
				  (cons (list name (quote '*unassigned*)) initforms)
				  (cons (list 'set! name value) body))))))))
	(make-body defines '() (reverse-list non-defines))))))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

; ex 4.20
(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (let ((initforms (let-inits exp))
	(body (let-body exp)))
    (make-let
     (map (lambda (initform)
	    (list (car initform)
		  (quote '*unassigned*)))
	  initforms)
     (append
      (map (lambda (initform)
	     (list 'set! (car initform) (cadr initform)))
	   initforms)
      body))))

; ex 4.21
(define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (f k)
       (if (= k 1)
	   1
	   (if (= k 2)
	       1
	       (+ (f f (- k 1))
		  (f f (- k 2)))))))))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
	 #t
	 (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
	 #f
	 (ev? ev? od? (- n 1))))))
