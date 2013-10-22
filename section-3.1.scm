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

; ex 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; ex 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; ex 3.14
(define (mystery x)
  (letrec ((loop (lambda (x y)
		   (if (null? x)
		       y
		       (let ((temp (cdr x)))
			 (set-cdr! x y)
			 (loop temp x))))))
    (loop x '())))

; ex 3.17
(define (count-pairs x)
  (letrec ((inner (lambda (x memo-list)
		    (if (and (pair? x)
			     (not (memq x memo-list)))
			(inner (car x)
			       (inner (cdr x)
				      (cons x memo-list)))
			memo-list))))
    (length (inner x '()))))

; ex 3.18
(define (has-loop? x)
  (letrec ((traverse-list (lambda (x memo-list)
			    (if (pair? x)
				(if (memq x memo-list)
				    #t
				    (let ((new-memo-list (cons x memo-list)))
				      (or (traverse-list (car x) new-memo-list) ; XXX
					  (traverse-list (cdr x) new-memo-list))))
				#f))))
    (traverse-list x '())))

; ex 3.19
; ???
(define (o1-space-has-loop? x)
  (letrec ((traverse-sub-list (lambda (start cur step)
				(if (eq? start cur)
				    #t
				    (if (or (not (pair? start)) (= step 1))
					#f
					(traverse-sub-list (cdr start) cur (- step 1)))))))
    (letrec ((traverse-list (lambda (cur count)
			      (if (traverse-sub-list x cur count)
				  #t
				  (if (pair? cur)
				      (traverse-list (cdr cur) (+ count 1))
				      #f)))))
      (if (pair? x)
	  (traverse-list (cdr x) 1)
	  #f))))

; 使用两个变量, 一个变量以步长为 1 遍历列表, 另一个变量以步长为 2 遍历列表,
; 每次在两个变量移动之后对比它们, 如果两个变量相遇，那么列表有环;
; 如果能走完整个列表(遇到 '() ), 那么列表没有环.
(define (another-o1-space-has-loop? lst)
  (define (iter x y)
    (let ((x-walk (list-walk 1 x))
	  (y-walk (list-walk 2 y)))
      (cond ((or (null? x-walk) (null? y-walk))
	     #f)
	    ((eq? x-walk y-walk)
	     #t)
	    (else
	     (iter x-walk y-walk)))))
  (iter lst lst))

(define (list-walk step lst)
  (cond ((null? lst)
	 '())
	((= step 0)
	 lst)
	(else
	 (list-walk (- step 1)
		    (cdr lst)))))
