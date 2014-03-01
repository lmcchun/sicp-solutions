; ex 3.28
(define (or-gate a1 a2 output)
  (let ((or-action-procedure
	 (lambda ()
	   (let ((new-value
		  (logical-or (get-signal a1) (get-signal a2))))
	     (after-delay or-gate-delay
			  (lambda ()
			    (set-signal! output new-value)))))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok))

; ex 3.29 ???
(define (or-gate-dm a1 a2 output)
  (let ((b1 (make-wire))
	(b2 (make-wire))
	(c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)
    'ok))

(define or-gate-dm-delay
  (+ inverter-delay
     and-gate-delay
     inverter-delay))

; ex 3.30
(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define half-adder-delay
  (max (+ or-gate-delay
	  and-gate-delay)
       (+ and-gate-delay
	  inverter-delay
	  and-gate-delay)))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (o-gate c1 c2 out)
    'ok))

(define full-adder-delay
  (+ half-adder-delay
     half-adder-delay
     or-gate-delay))

(define (ripple-carry-adder al bl sl c)
  (let ((c-in (make-wire)))
    (if (null? (cdr al))
	(set-signal! c-in 0)
	(ripple-carry-adder (cdr al) (cdr bl) (cdr sl) c-in))
    (full-adder (car al) (car bl) c-in (car sl) c)))

(define ripple-carry-adder-delay
  (* n full-adder-delay))

;
(define (make-wire)
  (let ((signal-value 0)
	(action-procedures '()))
    (let* ((set-my-signal!
	    (lambda (new-value)
	      (if (not (= signal-value new-value))
		  (begin (set! signal-value new-value)
			 (call-each action-procedures))
		  'done)))
	   (accept-action-procedure!
	    (lambda (proc)
	      (set! action-procedures (cons proc action-procedures))
	      (proc)))
	   (dispatch
	    (lambda (m)
	      (cond ((eq? m 'get-signal) signal-value)
		    ((eq? m 'set-signal!) set-my-signal!)
		    ((eq? m 'add-action!) accept-action-procedure!)
		    (else (error "Unknown operation -- WIRE" m))))))
      dispatch)))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
	((car procedures))
	(call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

;
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s)
  (car s))

(define (segment-queue s)
  (cdr s))

(define (make-agenda)
  (list 0))

(define (current-time agenda)
  (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda)
  (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (letrec ((belongs-before?
	    (lambda (segments)
	      (or (null? segments)
		  (< time (segment-time (car segments))))))
	   (make-new-time-segment
	    (lambda (time action)
	      (let ((q (make-queue)))
		(insert-queue! q action)
		(make-time-segment time q))))
	   (add-to-segments!
	    (lambda (segments)
	      (if (= (segment-time (car segments)) time)
		  (insert-queue! (segment-queue (car segments))
				 action)
		  (let ((rest (cdr segments)))
		    (if (belongs-before? rest)
			(set-cdr!
			 segments
			 (cons (make-new-time-segment time action)
			       (cdr segments)))
			(add-to-segments! rest)))))))
    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
	  (set-segments!
	   agenda
	   (cons (make-new-time-segment time action)
		 segments))
	  (add-to-segments! segments)))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))
