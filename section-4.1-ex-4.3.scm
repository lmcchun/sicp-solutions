(define (make-table)
  (let ((local-table (list '*table*)))
    (let* ((lookup
	    (lambda (key)
	      (let ((record (assoc key (cdr local-table))))
		(if record
		    (cdr record)
		    #f))))
	   (insert!
	    (lambda (key value)
	      (let ((record (assoc key (cdr local-table))))
		(if record
		    (set-cdr! record value)
		    (set-cdr! local-table
			      (cons (cons key value)
				    (cdr local-table)))))))
	   (dispatch
	    (lambda (m)
	      (cond ((eq? m 'lookup-proc) lookup)
		    ((eq? m 'insert-proc!) insert!)
		    (else
		     (error "Unknown operation -- TABLE" m))))))
      dispatch)))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define (expression-type exp)
  (car exp))

(define (expression-content exp)
  (cdr exp))

(define (self-evaluting? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))

(define (eval exp env)
  (if (self-evaluting? exp)
      exp
      (let ((eval-proc (get (expression-type exp))))
	(eval-proc (expression-content exp)
		   env))))

; define lookup-variable-value TODO

(define (install-variable)
  (put 'variable
       (lambda (exp env)
	 (lookup-variable-value exp env))))

(define (install-quoted)
  (let ((text-of-quotation
	 (lambda (exp env)
	   (cadr exp))))
    (put 'quote text-of-quotation)))

; define set-variable-value! TODO

(define (install-assignment)
  (let* ((assignment-variable
	  (lambda (exp) (cadr exp)))
	 (assignment-value
	  (lambda (exp) (caddr exp)))
	 (eval-assignment
	  (lambda (exp env)
	    (set-variable-value! (assignment-variable exp)
				 (eval (assignment-value exp) env)
				 env)
	    'ok)))
    (put 'set! eval-assignment)))

; define define-variable! TODO

(define (install-define)
  (let* ((definition-variable
	   (lambda (exp)
	     (if (symbol? (cadr exp))
		 (cadr exp)
		 (caddr exp))))
	 (definition-value
	   (lambda (exp)
	     (if (symbol? (cadr exp))
		 (caddr exp)
		 (make-lambda (cdadr exp) ; formal parameters
			      (cddr exp))))) ; body
	 (eval-definition
	  (lambda (exp env)
	    (define-variable!
	      (definition-variable exp)
	      (eval (definition-value exp) env)
	      env)
	    'ok)))
    (put 'define eval-definition)))

; define true? TODO

(define (install-if)
  (let* ((if-predicate
	  (lambda (exp) (cadr exp)))
	 (if-consequent
	  (lambda (exp) (caddr exp)))
	 (if-alternative
	  (lambda (exp)
	    (if (not (null? (cdddr exp)))
		(cadddr exp)
		#f)))
	 (eval-if
	  (lambda (exp env)
	    (if (true? (eval (if-predicate exp) env))
		(eval (if-consequent exp) env)
		(eval (if-alternative exp) env)))))
    (put 'if eval-if)))

; define make-procedure TODO

(define (install-lambda)
  (let* ((lambda-parameters
	  (lambda (exp) (cadr exp)))
	 (lambda-body
	  (lambda (exp) (cddr exp))))
    (make-procedure (lambda-parameters exp)
		    (lambda-body exp)
		    env)))
