(define (another-memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (another-memq item (cdr x)))))

; ex 2.54
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (another-equal? s1 s2)
  (cond ((and (atom? s1) (atom? s2)) (eq? s1 s2))
        ((or (atom? s1) (atom? s2)) #f)
        ((and (null? s1) (null? s2)) #t)
        ((or (null? s1) (null? s2)) #f)
        (else (and (equal? (car s1) (car s2))
                   (equal? (cdr s1) (cdr s2))))))

;
(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (eq? v1 v2))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unkonwn expression type -- DERIV" exp))))