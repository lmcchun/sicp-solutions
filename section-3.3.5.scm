(define (adder a1 a2 sum)
  (let* ((process-new-value
	  (lambda ()
	    (cond ((and (has-value? a1) (has-value? a2))
		   (set-value! sum
			       (+ (get-value a1) (get-value a2))
			       me))
		  ((and (has-value? a1) (has-value? sum))
		   (set-value! a2
			       (- (get-value sum) (get-value a2))
			       me))
		  ((and (has-value? a2) (has-value? sum))
		   (set-value! a1
			       (- (get-value sum) (get-value a2))
			       me)))))
	 (process-forget-value
	  (lambda ()
	    (forget-value! sum me)
	    (forget-value! a1 me)
	    (forget-value! a2 me)
	    (process-new-value)))
	 (me
	  (lambda (request)
	    (cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
		   (error "Unknown request -- ADDER" request))))))
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (let* ((process-new-value
	  (lambda ()
	    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
		       (and (has-value? m2) (= (get-value m2) 0)))
		   (set-value! product 0 me))
		  ((and (has-value? m1) (has-value? m2))
		   (set-value! product
			       (* (get-value m1) (get-value m2))
			       me))
		  ((and (has-value? product) (has-value? m1))
		   (set-value! m2
			       (/ (get-value product) (get-value m1))
			       me))
		  ((and (has-value? product) (has-value? m2))
		   (set-value! m1
			       (/ (get-value product) (get-value m2))
			       me)))))
	 (process-forget-value
	  (lambda ()
	    (forget-value! product me)
	    (forget-value! m1 me)
	    (forget-value! m2 me)
	    (process-new-value)))
	 (me
	  (lambda (request)
	    (cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
		   (error "Unknown request -- MULTIPLIER" request))))))
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me))

(define (constant value connector)
  (let ((me
	 (lambda (request)
	   (error "Unknown request -- CONSTANT" request))))
    (connect connector me)
    (set-value! connector value me)
    me))

(define (probe name connector)
  (let* ((print-probe
	  (lambda (value)
	    (newline)
	    (display "Probe: ")
	    (display name)
	    (display " = ")
	    (display value)))
	 (process-new-value
	  (lambda ()
	    (print-probe (get-value connector))))
	 (process-forget-value
	  (lambda ()
	    (print-probe "?")))
	 (me
	  (lambda (request)
	    (cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
		   (error "Unknown request -- PROBE" request))))))
    (connect connector me)
    me))

(define (make-connector)
  (let ((value false)
	(informant false)
	(constraints '()))
    (let* ((set-my-value
	    (lambda (newval setter)
	      (cond ((not (has-value? me))
		     (set! value newval)
		     (set! informant setter)
		     (for-each-except setter
				      inform-about-value
				      constraints))
		    ((not (= value newval))
		     (error "Contradiction" (list value newval)))
		    (else 'ignored))))
	   (forget-my-value
	    (lambda (retractor)
	      (if (eq? retractor informant)
		  (begin (set! informant false)
			 (for-each-except retractor
					  inform-about-no-value
					  constraints))
		  'ignored)))
	   (connect
	    (lambda (new-constraint)
	      (if (not (memq new-constraint constraints))
		  (set! constraints
			(cons new-constraint constraints)))
	      (if (has-value? me)
		  (inform-about-value new-constraint))
	      'done))
	   (me
	    (lambda (request)
	      (cond ((eq? request 'has-value?)
		     (if informant true false))
		    ((eq? request 'value) value)
		    ((eq? request 'set-value!) set-my-value)
		    ((eq? request 'forget) forget-my-value)
		    ((eq? request 'connect) connect)
		    (else (error "Unknown operation -- CONNECTOR"
				 request))))))
      me)))

(define (for-each-except exception procedure list)
  (letrec ((loop
	    (lambda (items)
	      (cond ((null? items) 'done)
		    ((eq? (car items) exception) (loop (cdr items)))
		    (else (procedure (car items))
			  (loop (cdr items)))))))
    (loop list)))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (averager a b c)
  (let ((u (make-connector))
	(v (make-connector)))
    (adder a b u)
    (multiplier c v u)
    (constant 2 v)
    'ok))

; ex 3.35
(define (squarer a b)
  (let* ((process-new-value
	  (lambda ()
	    (if (has-value? b)
		(if (< (get-value b) 0)
		    (error "sequare less than 0 -- SEQUARE" (get-value b))
		    (set-value! a
				(sqrt (get-value b))
				me))
		(if (has-value? a)
		    (set-value! b
				(square (get-value a))
				me)))))
	 (process-forget-value
	  (lambda ()
	    (forget-value! a)
	    (forget-value! b)
	    (process-new-value)))
	 (me
	  (lambda (request)
	    (cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
		   (error "Unknown request -- SQUARER" request))))))
    (connect a me)
    (connect b me)
    me))

; ex 3.37
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((y (make-connector)))
    (constant x y)
    y))

;
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (let ((serialized-p
	     (lambda args
	       (mutex 'acquire)
	       (let ((val (apply p args)))
		 (mutex 'release)
		 val))))
	serialized-p))))

(define (make-mutex)
  (let ((cell (list false)))
    (letrec ((the-mutex
	      (lambda (m)
		(cond ((eq? m 'acquire)
		       (if (test-and-set! cell)
			   (the-mutex 'acquire))) ; retry
		      ((eq? m 'release) (clear! cell))))))
      the-mutex)))

(define (clear! cell)
  (set-car cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
	     false)))

; 时间片模型
(define (timeslice-test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
	 true
	 (begin (set-car! cell true)
		false)))))

; ex 3.47
; a)
(define (make-semaphore-mutex n)
  (let ((mutex (make-mutex)))
    (letrec ((acquire
	      (lambda ()
		(mutex 'acquire)
		(if (> n 0)
		    (begin (set! n (- n 1))
			   (mutex 'release)
			   'ok)
		    (begin (mutex 'release)
			   (acquire)))))
	     (release
	      (lambda ()
		(mutex 'acquire)
		(set! n (+ n 1))
		(mutex 'release)
		'ok))
	     (dispatch
	      (lambda (m)
		(cond ((eq? m 'acquire)
		       (acquire))
		      ((eq? m 'release)
		       (release))
		      (else
		       (error "Unknown request -- DISPATCH" m))))))
      dispatch)))

; b)
(define (semaphore-test-and-set! cell m)
  (let ((n (car cell)))
    (cond ((eq? m 'acquire)
	   (if (= n 0)
	       true
	       (begin (set-car! cell (- n 1))
		      false)))
	  ((eq? m 'release)
	   (begin (set-car! cell (+ n 1))
		  false))
	  (error "Unknown request -- TEST-AND-SET!" m))))

(define (make-semaphore n)
  (let ((cell (list n)))
    (letrec ((the-semaphore
	      (lambda (m)
		(cond ((eq? m 'acquire)
		       (if (semaphore-test-and-set! cell m)
			   (the-semaphore 'acquire))) ; retry
		      ((eq? m 'release)
		       (semaphore-test-and-set! cell m))
		      (else
		       (error "Unknown request -- SEMAPHORE" m))))))
      the-semaphore)))

; ex 3.48
(define (make-account balance)
  (let ((id (generate-account-id))) ; 生成遞增的 id
    (let ((withdraw
	   (lambda (amount)
	     (if (>= balance amount)
		 (begin (set! balance (- balance amount))
			balance)
		 "Insufficient funds")))
	  (deposit
	   (lambda (amount)
	     (set! balance (+ balance amount))
	     balance)))
      (let ((balance-serializer (make-serializer)))
	(let ((dispatch
	       (lambda (m)
		 (cond ((eq? m 'withdraw) withdraw)
		       ((eq? m 'deposit) deposit)
		       ((eq? m 'balance) balance)
		       ((eq? m 'serializer) balance-serializer)
		       ((eq? m 'id) id)
		       (else
			(error "Unknown request -- DISPATCH" m))))))
	  dispatch)))))

(define (serialized-exchange acc-1 acc-2)
  (if (< (acc-1 'id) (acc-2 'id))
      (serialized-and-exchange acc-1 acc-2)
      (serialized-and-exchange acc-2 acc-1)))

(define (serialized-and-exchange smaller-id-account bigger-id-account)
  (let ((smaller-serializer (smaller-id-account 'serializer)))
    (let ((bigger-serializer (bigger-id-account 'serializer)))
      ((bigger-serializer (smaller-serializer exchange))
       smaller-id-account
       bigger-id-account))))
