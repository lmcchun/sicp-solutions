;
(define (find-binding-in-frame var frame)
  "Looks up the variable in the frame.
  Returns a pair: if the -car- of the pair is t,
  then the variable was found and it's in the -cdr-
  of the pair. If the -car- of the pair is nil,
  then the variable was not found"
  (letrec ((scan
	    (lambda (vars vals)
	      (cond ((null? vars)
		     (cons #f '()))
		    ((eq? var (car vars))
		     (cons #t (car vals)))
		    (else
		     (scan (cdr vars) (cdr vals)))))))
    (scan (frame-variables frame)
	  (frame-values frame))))

(define (set-binding-in-frame! var val frame)
  "Sets the variable to the value in the frame.
  Returns t if the variable was found and modified,
  nil otherwise."
  (letrec ((scan
	    (lambda (vars vals)
	      (cond ((null? vars) #f)
		    ((eq? var (car vars))
		     (set-car! vals val)
		     #t)
		    (else
		     (scan (cdr vars) (cdr vals)))))))
    (scan (frame-variables frame)
	  (frame-values frame))))

(define (lookup-variable-value var env)
  (letrec ((env-loop
	    (lambda (env)
	      (if (eq? env the-empty-environment)
		  (error "Unbound variable" var)
		  (let ((result
			 (find-binding-in-frame (first-frame) var)))
		    (if (car result)
			(cdr result)
			(env-loop (enclosing-environment env))))))))
    (env-loop env)))

(define (set-variable-value! var val env)
  (letrec ((env-loop
	    (lambda (env)
	      (if (eq? env the-empty-environment)
		  (error "Unbound variable" var)
		  (if (set-binding-in-frame! (first-frame env) var val)
		      #t
		      (env-loop (enclosing-environment env)))))))
    (env-loop env)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (set-binding-in-frame! frame var val)
	#t
	(add-binding-to-frame! frame var val))))
