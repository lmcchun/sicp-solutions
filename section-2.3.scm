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

;
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1)
                 (adjoin-set (car set1) set2))))

;
(define (element-of-sort-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-sort-set? x (cdr set)))))

(define (intersection-sort-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-sort-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-sort-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-sort-set set1 (cdr set2)))))))

(define (adjoin-sort-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-sort-set x (cdr set))))))

(define (union-sort-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((a1 (car set1))
                    (a2 (car set2)))
                (cond ((= a1 a2)
                       (cons a1 (union-sort-set (cdr set1)
                                                (cdr set2))))
                      ((< a1 a2)
                       (cons a1 (union-sort-set (cdr set1) set2)))
                      ((< a2 a1)
                       (cons a2 (union-sort-set set1 (cdr set2)))))))))

;
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-tree? x (left-branch set)))
        ((> x (entry set))
         (element-of-tree? x (right-branch set)))))

(define (adjoin-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-tree x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-tree x (right-branch set))))))

; ex 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (letrec ((copy-to-list
            (lambda (tree result-list)
              (if (null? tree)
                  result-list
                  (copy-to-list (left-branch tree)
                                (cons (entry tree)
                                      (copy-to-list (right-branch tree)
                                                    result-list)))))))
    (copy-to-list tree '())))

; ex 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; ex 2.65
(define (intersection-set-??? set1 set2)
  ;;; 失败的做法
  (letrec ((iter
            (lambda (set1 set2 lst)
              (if (or (null? set1)
                      (null? set2))
                  lst
                  (let ((x1 (car set1))
                        (x2 (car set2)))
                    (cond ((> x1 x2)
                           (iter (left-branch set1)
                                 set2
                                 (iter (make-tree x1 '() (right-branch set1))
                                       (right-branch set2)
                                       lst)))
                          ((< x1 x2)
                           (iter (make-tree x1 (left-branch set1) '())
                                 (left-branch set2)
                                 (iter (right-branch set1)
                                       set2
                                       lst)))
                          ((= x1 x2)
                           (iter (left-branch set1)
                                 (left-branch set2)
                                 (cons x1 (iter (right-branch set1)
                                                (right-branch set2)
                                                lst))))))))))
    (list->tree (iter set1 set2 '()))))

(define (intersection-tree tree1 tree2)
  (list->tree
   (intersection-sort-set (tree->list-2 tree1)
                          (tree->list-2 tree2))))

(define (union-tree tree1 tree2)
  (list->tree
   (union-sort-set (tree->list-2 tree1)
                   (tree->list-2 tree2))))

; ex 2.66
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((x (entry set-of-records))
            (x-key (key x)))
        (cond ((< given-key x-key)
               (lookup given-key (left-branch set-of-records)))
              ((> given-key x-key)
               (lookup given-key (right-branch set-of-records)))
              ((= given-key x-key) x)))))

;
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (letrec ((decode-1
            (lambda (bits current-branch)
              (if (null? bits)
                  '()
                  (let ((next-branch
                         (choose-branch (car bits) current-branch)))
                    (if (leaf? next-branch)
                        (cons (symbol-leaf next-branch)
                              (decode-1 (cdr bits) tree))
                        (decode-1 (cdr bits) next-branch)))))))
    (decode-1 bits tree)))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-huffman-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-huffman-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-huffman-set (make-leaf (car pair) ; symbol
                                       (cadr pair)) ; frequency
                            (make-leaf-set (cdr pairs))))))

; ex 2.67
(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; ex 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (letrec ((encode-symbol-1
            (lambda (tree)
              (if (leaf? tree)
                  '()
                  (let ((left (left-branch tree)))
                    (if (memq symbol (symbols left))
                        (cons 0 (encode-symbol-1 left))
                        (let ((right (right-branch tree)))
                          (if (memq symbol (symbols right))
                              (cons 1 (encode-symbol symbol right))
                              (error "Error!" symbol)))))))))
    (encode-symbol-1 tree)))

; ex 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
        ((= (length leaf-set) 1) (car leaf-set))
        (else
         (successive-merge
          (adjoin-huffman-set
           (make-code-tree (car leaf-set)
                           (cadr leaf-set))
           (cddr leaf-set))))))

; ex 2.70
(define sample-pairs
  '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define song
  '(Get a job
        Sha na na na na na na na na
        Get a job
        Sha na na na na na na na na
        Wah yip yip yip yip yip yip yip yip yip
        Sha boom))

; ex 2.71
(define sample-pairs-1
  '((A 1) (B 2) (C 4) (D 8) (E 16)))

(define sample-pairs-2
  '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))

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