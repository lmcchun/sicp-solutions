;
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sina a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (anglle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (make-from-real-imag x y)
  (make-from-real-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

(define (install-rectangular-package)
  ;;; internal procedures
  (let* ((real-part (lambda (z) (car z)))
         (imag-part (lambda (z) (cdr z)))
         (make-from-real-imag (lambda (x y) (cons x y)))
         (magnitude (lambda (z)
                      (sqrt (+ (square (real-part z))
                               (square (imag-part z))))))
         (angle (lambda (z) (atan (imag-part z) (real-part z))))
         (make-from-mag-ang (lambda (r a)
                              (cons (* r (cos a)) (* r (sin a))))))
    ;;; interface to the rest of the system
    (let ((tag (lambda (x) (attach-tag 'rectangular x))))
      (put 'real-part '(rectangular) real-part)
      (put 'imag-part '(rectangular) imag-part)
      (put 'magnitude '(rectangular) magnitude)
      (put 'angle '(rectangular) angle)
      (put 'make-from-real-imag 'rectangular
           (lambda (x y) (tag (make-from-real-imag x y))))
      (put 'make-from-mag-ang 'rectangular
           (lambda (r a) (tag (make-from-mag-ang r a))))
      'done)))

(define (install-polar-package)
  ;;; internal procedures
  (let* ((magnitude (lambda (z) (car z)))
         (angle (lambda (z) (cdr z)))
         (make-from-mag-ang (lambda (r a) (cons r a)))
         (real-part (lambda (z)
                      (* (magnitude z) (cos (angle z)))))
         (imag-part (lambda (z)
                      (* (magnitude z) (sin (angle z)))))
         (make-from-real-imag (lambda (x y)
                                (cons (sqrt (+ (square x) (square y)))
                                      (atan y x)))))
    ;;; interface to the rest of the system
    (let ((tag (lambda (x) (attach-tag 'polar x))))
      (put 'real-part '(polar) real-part)
      (put 'imag-part '(polar) imag-part)
      (put 'magnitude '(polar) magnitude)
      (put 'angle '(polar) angle)
      (put 'make-from-real-imag 'polar
           (lambda (x y) (tag (make-from-real-imag x y))))
      (put 'make-from-mag-ang 'polar
           (lambda (r a) (tag (make-from-mag-ang r a))))
      'done)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No methon for these types -- APPLY-GENERIC" (list op type-tags))))))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; ex 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (install-sum-package)
  ;;; internal procedures
  (let* ((addend (lambda (s) (car s)))
         (augend (lambda (s) (cadr s)))
         (make-sum (lambda (x y)
                     (cond ((=number? x 0) y)
                           ((=number? y 0) x)
                           ((and (number? x) (number? y)) (+ x y))
                           (else (attach-tag '+ (list x y))))))
         (deriv (lambda (exp var)
                  (make-sum (deriv (addend exp) var)
                            (deriv (augend exp) var)))))
    ;;; interface to the rest of the system
    (put 'addend '+ addend)
    (put 'augend '+ augend)
    (put 'make-sum '+ make-sum)
    (put 'deriv '+ deriv)
    'done))

(define (addend sum)
  ((get 'addend '+) (contents sum)))

(define (augend sum)
  ((get 'augend '+) (contents sum)))

(define (make-sum x y)
  ((get 'make-sum '+) x y))

(define (install-product-package)
  ;;; internal procedures
  (let* ((multiplier (lambda (s) (car s)))
         (multiplicand (lambda (s) (cadr s)))
         (make-product (lambda (x y)
                         (cond ((or (=number? x 0) (=number? y 0)) 0)
                               ((=number? x 1) y)
                               ((=number? y 1) x)
                               (else (attach-tag '* (list x y))))))
         (deriv (lambda (exp var)
                  (make-sum
                   (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))))
    ;;; interface to the rest of the system
    (put 'multiplier '* multiplier)
    (put 'multiplicand '* multiplicand)
    (put 'make-product '* make-product)
    (put 'deriv '* deriv)
    'done))

(define (multiplier s)
  ((get 'multiplier '*) (contents s)))

(define (multiplicand s)
  ((get 'multiplicand '*) (contents s)))

(define (make-product x y)
  ((get 'make-product '*) x y))

(define (install-exponentiation-package)
  ;;; internal procedures
  (let* ((base (lambda (s) (car s)))
         (exponent (lambda (s) (cadr s)))
         (make-exponentiation (lambda (b e)
                                (cond ((= e 0) 1)
                                      ((= e 1) b)
                                      ((and (number? b) (number? e)) (expt b e))
                                      (else (attach-tag '** (list b e))))))
         (deriv (lambda (exp var)
                  (let ((b (base exp))
                        (e (exponent exp)))
                    (make-product e
                                  (make-product
                                   (make-exponentiation b (- e 1))
                                   (deriv b var)))))))
    ;;; interface to the rest of the system
    (put 'base '** base)
    (put 'exponent '** exponent)
    (put 'make-exponentiation '** make-exponentiation)
    (put 'deriv '** deriv)
    'done))

(define (base s)
  ((get 'base '**) (contents s)))

(define (exponent s)
  ((get 'exponent '**) (contents s)))

(define (make-exponentiation b e)
  ((get 'make-exponentiation '**) b e))

; ex 2.74
(define (make-generic-file division file)
  (list division file))

(define (division-of-genericc-file gf)
  (car gf))

(define (file-of-generic-file gf)
  (cadr gf))

(define (get-record employee file)
  ((get 'get-record
        (division-of-generic-file file))
   employee
   (file-of-generic-file file)))

(define (get-salary employee)
  ((get 'get-salary
        (division-of-generic-employee employee))
   (employee-of-generic-employee employee)))

(define (find-employee-record employee file-list)
  (if (null? file-list)
      '()
      (let ((record (get-record employee (car file-list))))
        (if (null? record)
            (find-employee-record employee (cdr file-list))
            record))))

;
(define (make-from-real-imag x y)
  (let ((dispatch (lambda (op)
                    (cond ((eq? op 'real-part) x)
                          ((eq? op 'imag-part) y)
                          ((eq? op 'magnitude)
                           (sqrt (+ (square x) (square y))))
                          ((eq? op 'angle) (atan y x))
                          (else
                           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))))
    dispatch))

; ex 2.75
(define (make-from-mag-ang r a)
  (let ((dispatch (lambda (op)
                    (cond ((eq? op 'magnitude) r)
                          ((eq? op 'angle) a)
                          ((eq? op 'real-part)
                           (* r (cos a)))
                          ((eq? op 'imag-part)
                           (* r (sin a)))
                          (else
                           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))))
    dispatch))

;
(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (let ((tag (lambda (x)
               (attach-tag 'scheme-number x))))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    (put 'equ? '(scheme-number scheme-number)
         (lambda (x y) (= x y)))
    (put '=zero? 'scheme-number
         (lambda (x) (= x 0)))
    'done))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;;; internal procedures
  (let* ((numer (lambda (x) (car x)))
         (denom (lambda (x) (cdr x)))
         (make-rat (lambda (n d)
                     (let ((g (gcd n d)))
                       (cons (/ n g) (/ d g)))))
         (add-rat (lambda (x y)
                    (make-rat (+ (* (numer x) (denom y))
                                 (* (numer y) (denom x)))
                              (* (denom x) (denom y)))))
         (sub-rat (lambda (x y)
                    (make-rat (- (* (numer x) (denom y))
                                 (* (numer y) (denom x)))
                              (* (denom x) (denom y)))))
         (mul-rat (lambda (x y)
                    (make-rat (* (numer x) (numer y))
                              (* (denom x) (denom y)))))
         (div-rat (lambda (x y)
                    (make-rat (* (numer x) (denom y))
                              (* (denom x) (numer y))))))
    (let ((tag (lambda (x) (attach-tag 'rational x))))
      (put 'add '(rational rational)
           (lambda (x y) (tag (add-rat x y))))
      (put 'sub '(rational rational)
           (lambda (x y) (tag (sub-rat x y))))
      (put 'mul '(rational rational)
           (lambda (x y) (tag (mul-rat x y))))
      (put 'div '(rational rational)
           (lambda (x y) (tag (div-rat x y))))
      (put 'make 'rational
           (lambda (n d) (tag (make-rat n d))))
      (put 'equ? '(rational rational)
           (lambda (x y) (and (= (numer x) (numer y))
                              (= (denom x) (denom y)))))
      (put '=zero? 'rational
           (lambda (x) (= (numer x) 0)))
      'done)))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;;; imported procedures from rectangular and polar packages
  (let* ((make-from-real-imag (lambda (x y)
                                ((get 'make-from-real-imag 'rectangular) x y)))
         (make-from-mag-ang (lambda (x y)
                              ((get 'make-from-mag-ang 'polar) r a)))
         (add-complex (lambda (z1 z2)
                        (make-from-real-imag (+ (real-part z1) (real-part z2))
                                             (+ (imag-part z1) (imag-part z2)))))
         (sub-complex (lambda (z1 z2)
                        (make-from-real-imag (- (real-part z1) (real-part z2))
                                             (- (imag-part z1) (imag-part z2)))))
         (mul-complex (lambda (z1 z2)
                        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                           (+ (agnle z1) (angle z2)))))
         (div-complex (lambda (z1 z2)
                        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                           (- (angle z1) (angle z2))))))
    ;;; interface to rest of the system
    (let ((tag (lambda (z) (attach-tag 'complex z))))
      (put 'add '(complex complex)
           (lambda (z1 z2) (tag (add-complex z1 z2))))
      (put 'sub '(complex complex)
           (lambda (z1 z2) (tag (sub-complex z1 z2))))
      (put 'mul '(complex complex)
           (lambda (z1 z2) (tag (mul-complex z1 z2))))
      (put 'div '(complex complex)
           (lambda (z1 z2) (tag (div-complex z1 z2))))
      (put 'make-from-real-imag 'complex
           (lambda (x y) (tag (make-from-real-imag x y))))
      (put 'make-from-mag-ang 'complex
           (lambda (x y) (tag (make-from-mag-ang r a))))
      (put 'real-part '(complex) real-part)
      (put 'imag-part '(complex) imag-part)
      (put 'magnitude '(complex) magnitude)
      (put 'angle '(complex) angle)
      (put 'equ? '(complex complex)
           (lambda (z1 z2) (and (= (real-part z1) (real-part z2))
                                (= (imag-part z1) (imag-part z2)))))
      (put '=zero? 'complex
           (lambda (z) (and (= (real-part z) 0)
                            (= (imag-part z) 0))))
      'done)))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; ex 2.78
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (attach-tag tag x)
  (if (and (eq? tag 'scheme-number)
           (number? x))
      x
      (cons tag x)))

(define (equ? x y) (apply-generic 'equ? x y))

(define (=zero? x) (apply-generic '=zero? x))

; ex 2.81
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                            (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types" (list op type-tags)))))))
              (error "No method for these types" (list op type-tags)))))))

; ex 2.82
(define (apply-generic op .args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (letrec ((f (lambda (target-type lst result)
                        (if (null? lst)
                            result
                            (let ((x (car lst))
                                  (source-type (type-tag x)))
                              (if (eq? source-type target-type)
                                  (f target-type (cdr lst) (cons x result))
                                  (let ((coercion (get-coercion source-type target-type)))
                                    (if coercion
                                        (f target-type (cdr lst) (cons (coercion x) result))
                                        '())))))))
                   (g (lambda (lst)
                        (if (null? lst)
                            (error "No method for these types" (list op type-tags))
                            (let ((coercion-args (f (type-tag (car lst)) args '())))
                              (if coercion-args
                                  (let ((proc1 (get op (map type-tag coercion-args))))
                                    (if proc1
                                        (apply proc1 (map contents coercion-args))
                                        (g (cdr lst))))))))))
            (g type-tags))))))

; ex 2.83
(define (integer->rational n)
  (make-rational n 1))

(define (rational->real r)
  (make-real
   (exact->inexact
    (/ (numer r) (denom r)))))

(define (real->complex r)
  (make-complex-from-real-imag r 0))

(define (install-raise)
  (put 'raise '(integer)
       (lambda (i) (integer->rational i)))
  (put 'raise '(rational)
       (lambda (r) (rational->real r)))
  (put 'raise '(real)
       (lambda (r) (real->complex r))))
  
(define (raise x)
  (apply-generic 'raise x))

; ex 2.84
(define (level type)
  (cond ((eq? type 'integer) 0)
        ((eq? type 'rational) 1)
        ((eq? type 'real) 2)
        ((eq? type 'complex) 3)
        (else (error "Invalid type: LEVEL" type))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((no-method (lambda ()
                             (error "No method for these types" (list op type-tags)))))
            (if (not (null? (cdr args))) ; length of args > 1
                (let ((raised-args (raise-to-common args)))
                  (if raised-args
                      (let ((proc (get op (map type-tag raised-args))))
                        (if proc
                            (apply proc (map contents raised-args))
                            (no-method)))
                      (no-method)))
                (no-method)))))))

(define (raise-to-common args)
  (let ((highest (highest-type args)))
    (let ((raised-args
           (map (lambda (x) (raise-to-type highest x)) args)))
      (if (all-true? raised-args)
          raised-args
          #f))))

(define (all-true? lst)
  (cond ((null? lst) #t)
        ((car lst) (all-true? (cdr lst)))
        (else #f)))

(define (raise-to-type type item)
  (let ((item-type (type-tag item)))
    (if (eq? item-type type)
        item
        (let ((raise-fn (get 'raise (list item-type))))
          (if raise-fn
              (raise-to-type type (raise-fn item))
              #f)))))

(define (highest-type args)
  (if (null? (cdr args))
      (type-tag (car args))
      (let ((t1 (type-tag (car args)))
            (t2 (highest-type (cdr args))))
        (let ((l1 (level t1))
              (l2 (level t2)))
          (if (> l1 l2)
              l1
              l2)))))

; ex 2.85
(define (install-project)
  (put 'project '(rational)
       (lambda (r)
         (make-scheme-number
          (round (/ (numer r) (denom r))))))
  (put 'project '(real)
       (lambda (r)
         (let ((scheme-rat
                (rationalize
                 (inexact->exact r) 1/100)))
           (make-rational
            (numerator scheme-rat)
            (denominator scheme-rat)))))
  (put 'project '(complex)
       (lambda (c) (make-real (real-part c)))))

(define (drop num)
  (let ((project-proc
         (get 'project (list (type-tag num)))))
    (if project-proc
        (let ((dropped (project-proc (contents num))))
          (if (equ? num (raise dropped))
              (drop dropped)
              num))
        num)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (let ((no-method (lambda ()
                             (error "No method for these types" (list op type-tags)))))
            (if (not (null? (cdr args))) ; length of args > 1
                (let ((raised-args (raise-to-common args)))
                  (if raised-args
                      (let ((proc (get op (map type-tag raised-args))))
                        (if proc
                            (drop (apply proc (map contents raised-args)))
                            (no-method)))
                      (no-method)))
                (no-method)))))))

; ex 2.86
(define (install-sine-cosine)
  (put 'sine 'scheme-number
       (lambda (x) (attach-tag 'scheme-number (sin x))))
  (put 'cosine 'scheme-number
       (lambda (x) (attach-tag 'scheme-number (cos x))))
  (put 'sine 'rational
       (lambda (x) (attach-tag 'rational (sin x))))
  (put 'cosine 'rational
       (lambda (x) (attach-tag 'rational (cos x)))))

(define (sine x) (apply-generic 'sine x)) 

(define (cosine x) (apply-generic cosine x)) 

;; To accomodate generic number in the complex package,  
;; we should replace operators such as + , * with theirs 
;; generic counterparts add, mul.
(define (add-complex z1 z2)
  (make-from-real-imag (add (real-part z1) (real-part z2))
                       (add (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (sub (real-part z1) (real-part z2))
                       (sub (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                     (add (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                     (sub (angle z1) (angle z2))))