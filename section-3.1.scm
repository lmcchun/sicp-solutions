; ex 3.1
(define (make-accumulator init)
  (let ((sum init))
    (lambda (num)
      (set! sum (+ sum num))
      sum)))

; ex 3.2
(define (make-monitored f)
  (let ((times 0))
    (lambda (param)
      (cond ((eq? param 'how-many-calls?) times)
	    ((eq? param 'reset-count) (set! times 0))
	    (else (let ((result (f param)))
		    (set! times (+ times 1))
		    result))))))

; ex 3.3
(define (make-account password balance)
  (let ((withdraw
	 (lambda (amount)
	   (if (>= balance amount)
	       (begin (set! balance (- balance amount))
		      balance)
	       "Insufficient funds")))
	(deposit
	 (lambda (amount)
	   (set! balance (+ balance withdraw)))))
    (lambda (p m)
      (if (eq? p password)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request -- MAKE-ACCOUNT" m)))
	  (error "Incorrect password" "")))))

; ex 3.4
(define (make-account password balance)
  (let ((withdraw
	 (lambda (amount)
	   (if (>= balance amount)
	       (begin (set! balance (- balance amount))
		      balance)
	       "Insufficient funds")))
	(deposit
	 (lambda (amount)
	   (set! balance (+ balance withdraw))))
	(incorrect-password-times 0)
	(call-the-cops (lambda () (error "Call the cops" ""))))
    (lambda (p m)
      (if (eq? p password)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request -- MAKE-ACCOUNT" m)))
	  (begin (set! incorrect-password-times (+ incorrect-password-times 1))
		 (if (>= incorrect-password-times 7)
		     (call-the-cops)
		     (error "Incorrect password" "")))))))

; ex 3.5
(define (monte-carlo trials experiment)
  (letrec ((iter (lambda (trials-remaining trials-passed)
		   (cond ((= trials-remaining 0)
			  (/ trials-passed trials))
			 ((experiment)
			  (iter (- trials-remaining 1) (+ trials-passed 1)))
			 (else
			  (iter (- trials-remaining 1) trials-passed))))))
    (iter trials 0)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((area (* (- x2 x1) (- y2 y1))))
    (let ((test (lambda ()
		  (P (random-in-range x1 x2)
		     (random-in-range y1 y2)))))
      (* area (monte-carlo trials test)))))

(define (estimate-pi trials)
  (let ((unit-pred (lambda (x y)
		     (<= (+ (square x) (square y)) 1))))
    (estimate-integral unit-pred -1.0 1.0 -1.0 1.0 trials)))

; ex 3.6
(define (rand-maker)
  (let ((x random-init))
    (lambda (command)
      (cond ((eq? command 'generate)
	     (set! x (rand-update x))
	     x)
	    ((eq? command 'reset)
	     (lambda (init)
	       (set! x init)))
	    (else
	     (error "Bad command -- RAND" command))))))

;(define rand (rand-maker))

; ex 3.7
(define (make-joint account password-a password-b)
  (lambda (p m)
    (if (eq? p password-b)
	(account password-a m)
	(error "Incorrect password" ""))))

; ex 3.8
(define f
  ((lambda ()
     (let ((init 1))
       (lambda (num)
	 (set! init (* init num))
	 init)))))
