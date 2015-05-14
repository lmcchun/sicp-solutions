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

(define (tagged-list? exp tag)
  (if (pair? exp)
     (eq? (car exp) tag)
     #f))

(define (text-of-quotation exp)
  (cadr exp))

(define (eval-quoted exp)
  (text-of-quotation exp))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                      (eval* (assignment-value exp) env)
                      env))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
     (cadr exp)
     (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
     (caddr exp)
     (make-lambda (cadar exp) ; formal parameters
                 (cddr exp)))) ; body

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval* (definition-value exp) env)
    env)
  'ok)

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
     (cadddr exp)
     'false)) ; 'false or #f

(define (eval-if exp env)
  (if (true? (eval* (if-predicate exp) env))
     (eval* (if-consequent exp) env)
     (eval* (if-alternative exp) env)))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (eval-lambda exp env)
  (make-procedure
   (lambda-parameters exp)
   (lambda-body exp)
   env))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval* (first-exp exps) env))
       (else (eval* (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))

(define (eval-cond exp env)
  (let ((exps (cdr exp))
        (iter
         (lambda (exps)
           (if (null? exps)
              'nil
              (if (true? (eval* (car (first-exp exps)) env))
                 (eval* (cadr (first-exp exps)) env)
                 (iter (rest-exps exps)))))))
    (iter exps)))

(put 'quote eval-quoted)
(put 'set! eval-assignment)
(put 'define eval-definition)
(put 'if eval-if)
(put 'lambda eval-lambda)
(put 'begin eval-begin)
(put 'cond eval-cond)

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
       ((string? exp) #t)
       (else #f)))

(define (variable? exp)
  (symbol? exp))

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp)
  (cadr exp))

(define (operands exp)
  (cddr exp))

(define (eval* exp env)
  (cond ((self-evaluating? exp) exp)
       ((variable? exp)
        (lookup-variable-value exp env))
       ((get (car exp))
        ((get (car exp)) exp env))
       ((application? exp)
        (apply (eval* (operator exp) env)
              (list-of-values (operands exp) env)))
       (else
        (error "Unknown expression in EVAL: " exp))))
