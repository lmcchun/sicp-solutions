(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((and? exp) (eval-and exp env)) ; ((and? exp) (eval (and->if exp) env))
	((or? exp) (eval-or exp env)) ; ((or? exp) (eval (or->if exp) env))
	((let? exp) (eval (let->combination exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; ex 4.1
(define (left-to-right-eval-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
	(cons first-value
	      (left-to-right-eval-list-of-values
	       (rest-operands exps) env)))))

(define (right-to-left-eval-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values
	     (right-to-left-eval-list-of-values
	      (rest-operands exps)
	      env)))
	(cons (eval (first-operand exps) env)
	      rest-values))))

;
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
		   (cddr exp)))) ; body

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-seq? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (cdr ops))

(define (rest-operands ops)
  (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond-if exp)
  (expand-cond-clauses (cond-clauses exp)))

(define (expand-cond-clauses clauses)
  (if (null? clauses)
      'false ; clause else no
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-cond-clauses rest))))))

; ex 4.5
(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (letrec ((iter
	    (lambda (operands)
	      (if (no-operands? operands)
		  'true
		  (let ((first (first-operand operands))
			(rest (rest-operands operands)))
		    (if (true? (eval first env))
			(if (null? rest)
			    first
			    (iter rest))
			'false))))))
    (iter (operands exp))))

(define (and->if exp)
  (expand-and-operands (operands exp)))

(define (expand-and-operands operands)
  (if (no-operands? operands)
      'true
      (let ((first (first-operand operands))
	    (rest (rest-operands operands)))
	(if (no-operands? rest)
	    (make-if first first 'false)
	    (make-if first
		     (expand-and-operands rest)
		     'false)))))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (letrec ((iter
	    (lambda (operands)
	      (if (no-operands? operands)
		  'false
		  (if (true? (eval (first-operand operands) env))
		      'true
		      (iter (rest-operands operands)))))))
    (iter (operands exp))))

(define (or->if exp)
  (expand-or-operands (operands exp)))

(define (expand-or-operands operands)
  (if (no-operands? operands)
      'false
      (let ((first (first-operand operands))
	    (rest (rest-operands operands)))
	(make-if first 'true (expand-or-operands rest)))))

; ex 4.6
(define (extended-cond-test clause)
  (car clause))

(define (extended-cond-recipient clause)
  (caddr clause))

(define (expand-cond-clauses clauses)
  (if (null? clauses)
      'false ; clause else no
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    (if (null? (cdr first))
		(make-if (cond-predicate first)
			 'true
			 (expand-cond-clauses rest))
		(if (eq? (cadr first) '=>) ; XXX
		    (if (null? (cddr first))
			(error "Error clause -- COND-IF" first)
			(list (make-lambda '(test) ; (expand-cond-clauses rest) 只求值一次. XXX
					   (make-if 'test
						    (list (extended-cond-recipient first)
							  'test)
						    (expand-cond-clauses rest)))
			      (extended-cond-test first)))
		    (make-if (cond-predicate first)
			     (sequence->exp (cond-actions first))
			     (expand-cond-clauses rest))))))))

; ex 4.6
(define (let? exp)
  (tagged-list? exp 'let))

(define (map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst))
	    (map proc (cdr lst)))))

(define (let-vars exp)
  (map car (cadr exp)))

(define (let-inits exp)
  (map cadr (cadr exp)))

(define (let-body exp)
  (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-vars exp)
		     (let-body exp))
	(let-inits exp)))

; ex 4.7
(define (let*-body exp)
  (cddr exp))

(define (make-let initforms body)
  (cons 'let (cons initforms body)))

(define (let*->nested-lets exp)
  (let ((body (let*-body exp)))
    (letrec ((make-rec-let
	      (lambda (initforms)
		(if (null? initforms)
		    (make-let '() body)
		    (make-let (car initforms)
			      (list (make-rec-let (cdr initforms))))))))
      (make-rec-let (cadr exp)))))
