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

(define (make-sum a1 . a2)
  (letrec ((iter
            (lambda (lst addend augend)
              (if (null? lst)
                  (cons addend augend)
                  (let ((x (car lst)))
                    (if (number? x)
                        (iter (cdr lst) (+ addend x) augend)
                        (iter (cdr lst) addend (cons x augend))))))))
    (let ((iter-result (iter (cons a1 a2) 0 '()))) ; iter-result 不可能是空表
      (cond ((= (length iter-result) 1) (car iter-result))
            ((= (car iter-result) 0)
             (let ((rest (cdr iter-result)))
               (if (= (length rest) 1)
                   (car rest)
                   (cons '+ rest))))
            (else (cons '+ iter-result))))))

(define (make-product m1 . m2)
  (letrec ((iter
            (lambda (lst multiplier multiplicand)
              (if (null? lst)
                  (cons multiplier multiplicand)
                  (let ((x (car lst)))
                    (if (number? x)
                        (iter (cdr lst) (* multiplier x) multiplicand)
                        (iter (cdr lst) multiplier (cons x multiplicand))))))))
    (let ((iter-result (iter (cons m1 m2) 1 '()))) ; iter-result 不可能是空表
      (cond ((= (length iter-result) 1) (car iter-result))
            (else (let ((x (car iter-result)))
                    (cond ((= x 0) 0)
                          ((= x 1) (let ((rest (cdr iter-result)))
                                     (if (= (length rest) 1)
                                         (car rest)
                                         (cons '* rest))))
                          (else (cons '* iter-result)))))))))
              

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (let ((result (cddr s)))
    (cond ((null? result) 0)
          ((= (length result) 1) (car result))
          (else (cons '+ result)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (let ((result (cddr p)))
    (cond ((null? result) 1)
          ((= (length result) 1) (car result))
          (else (cons '* result)))))

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
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unkonwn expression type -- DERIV" exp))))

; ex 2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation b e)
  (cond ((= e 0) 1)
        ((= e 1) base)
        ((and (number? b) (number? e))
         (expt b e))
        (else (list '** b e))))

; ex 2.58
(define (make-sum-1 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum?-1 x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend-1 s) (car s))

(define (augend-1 s) (caddr s))

(define (make-product-1 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product?-1 x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier-1 p)
  (car p))

(define (multiplicand-1 p)
  (caddr p))

(define (deriv-1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum?-1 exp)
         (make-sum-1 (deriv-1 (addend-1 exp) var)
                     (deriv-1 (augend-1 exp) var)))
        ((product?-1 exp)
         (make-sum-1
          (make-product-1 (multiplier-1 exp)
                          (deriv-1 (multiplicand-1 exp) var))
          (make-product-1 (deriv-1 (multiplier-1 exp) var)
                          (multiplicand-1 exp))))
        (else
         (error "unkonwn expression type -- DERIV" exp))))