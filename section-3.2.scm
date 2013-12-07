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
