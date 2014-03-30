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
