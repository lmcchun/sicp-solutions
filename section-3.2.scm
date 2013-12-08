; ex 3.21
(define (print-queue queue)
  (car queue))

; ex 3.22
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (let ((set-front-ptr!
	   (lambda (item)
	     (set! front-ptr item)))
	  (set-rear-ptr!
	   (lambda (item)
	     (set! rear-ptr item))))
      (let* ((empty-queue?
	      (lambda ()
		(null? front-ptr)))
	     (front-queue
	      (lambda ()
		(if (empty-queue?)
		    (error "FRONT called with an empty queue" "")
		    (car front-ptr))))
	     (insert-queue!
	      (lambda (item)
		(let ((new-pair (cons item '())))
		  (cond ((empty-queue?)
			 (set-front-ptr! new-pair)
			 (set-rear-ptr! new-pair))
			(else
			 (set-cdr! rear-ptr new-pair)
			 (set-rear-ptr! new-pair))))))
	     (delete-queue!
	      (lambda ()
		(if (empty-queue?)
		    (error "DELETE! called with an empty queue" "")
		    (set-front-ptr! (cdr front-ptr))))))
	(lambda (m)
	  (cond ((eq? m 'empty-queue?) empty-queue?)
		((eq? m 'front-queue) front-queue)
		((eq? m 'insert-queue!) insert-queue!)
		((eq? m 'delete-queue!) delete-queue!)
		(else (error "Unknown operation -- DISPATCH" m))))))))

; ex 3.23
(define (make-deque)
  (cons '() '()))

(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (set-front-ptr! deque item)
  (set-car! deque item))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (caar (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (caar (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-cdr! new-pair (front-ptr deque))
	   (set-cdr! (car (front-ptr deque)) new-pair)
	   (set-front-ptr! deque new-pair)
	   deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-cdr! (car new-pair) (rear-ptr deque))
	   (set-cdr! (rear-ptr deque) new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "FRONT-DELETE-DEQUE! called with an empty deque" deque))
	(else
	 (set-front-ptr! deque (cdr (front-ptr deque)))
	 (set-cdr! (car (front-ptr deque)) '())
	 deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "REAR-DELETE-DEQUE! called with an empty deque" deque))
	(else
	 (set-rear-ptr! deque (cdar (rear-ptr deque)))
	 (set-cdr! (rear-ptr deque) '()))))

(define (print-deque deque)
  (if (empty-deque? deque)
      '()
      (let ((lst (front-ptr deque)))
	(letrec ((f (lambda (lst)
		      (if (null? lst)
			  '()
			  (cons (caar lst)
				(f (cdr lst)))))))
	  (f lst)))))

;

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	#f)))

(define (assoc key records)
  (cond ((null? records) #f)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      #f))
	#f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr1 record value)
	      (set-cdr1 subtable
			(cons (cons key-2 table)
			      (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (let ((lookup
	   (lambda (key-1 key-2)
	     (let ((subtable (assoc key-1 (cdr local-table))))
	       (if subtable
		   (let ((record (assoc key-2 (cdr subtable))))
		     (if record
			 (cdr record)
			 #f))
		   #f))))
	  (insert!
	   (lambda (key-1 key-2 value)
	     (let ((subtable (assoc key-1 (cdr local-table))))
	       (if subtable
		   (let ((record (assoc key-2 (cdr subtable))))
		     (if record
			 (set-cdr! record value)
			 (set-cdr! subtable
				   (cons (cons key-2 value)
					 (cdr subtable)))))
		   (set-cdr! local-table
			     (cons (list key-1
					 (cons key-2 value))
				   (cdr local-table))))))
	   'ok))
      (lambda (m)
	(cond ((eq? m 'lookup-proc) lookup)
	      ((eq? m 'insert-proc!) insert!)
	      (else (error "Unknown operation -- TABLE" m)))))))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(define (make-table some-key?)
  (let ((local-table (list '*table*))
	(assoc (lambda key records)
	       (cond ((null? records) #f)
		     ((some-key? key (caar records)) (car records)) ; equal? => some-key?
		     (else (assoc key (cdr records))))))
    (let ((lookup
	   (lambda (key-1 key-2)
	     (let ((subtable (assoc key-1 (cdr local-table))))
	       (if subtable
		   (let ((record (assoc key-2 (cdr subtable))))
		     (if record
			 (cdr record)
			 #f))
		   #f))))
	  (insert!
	   (lambda (key-1 key-2 value)
	     (let ((subtable (assoc key-1 (cdr local-table))))
	       (if subtable
		   (let ((record (assoc key-2 (cdr subtable))))
		     (if record
			 (set-cdr! record value)
			 (set-cdr! subtable
				   (cons (cons key-2 value)
					 (cdr subtable)))))
		   (set-cdr! local-table
			     (cons (list key-1
					 (cons key-2 value))
				   (cdr local-table))))))
	   'ok))
      (lambda (m)
	(cond ((eq? m 'lookup-proc) lookup)
	      ((eq? m 'insert-proc!) insert!)
	      (else (error "Unknown operation -- TABLE" m)))))))

