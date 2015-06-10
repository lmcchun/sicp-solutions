(define (error error-msg value)
  (display error-msg)
  (display #\space)
  (display value)
  (display #\newline))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
       ((string? exp) #t)
       (else #f)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (tagged-list? exp tag)
  (if (pair? exp)
     (eq? (car exp) tag)
     #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (if? exp)
  (tagged-list? exp 'if))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (application? exp)
  (pair? exp))

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (eval* exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
       ((quoted? exp) (analyze-quoted exp))
       ((variable? exp) (analyze-variable exp))
       ((assignment? exp) (analyze-assignment exp))
       ((definition? exp) (analyze-definition exp))
       ((if? exp) (analyze-if exp))
       ((lambda? exp) (analyze-lambda exp))
       ((begin? exp) (analyze-sequence (begin-actions exp)))
       ((cond? exp) (analyze (cond->if exp)))
       ((application? exp) (analyze-application exp))
       (else
        (error "Unknown expression type -- ANALYZE" exp))))