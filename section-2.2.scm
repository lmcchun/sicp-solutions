; ex 2.17
(define (last-pair lst)
  (let ((tail (cdr lst)))
    (if (null? tail)
        (car lst)
        (last-pair tail))))

; ex 2.18
(define (my-reverse lst)
  (letrec ((reverse-iter
            (lambda (rest reverse-lst)
              (if (null? rest)
                  reverse-lst
                  (reverse-iter (cdr rest)
                                (cons (car rest) reverse-lst))))))
    (reverse-iter lst '())))

; ex 2.19
(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? lst)
  (null? lst))

(define (first-denomination lst)
  (car lst))

(define (except-first-denomination lst)
  (cdr lst))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

; ex 2.20
(define (same-parity num . lst)
  (letrec ((same-parity-iter
            (lambda (num lst)
              (if (null? lst)
                  '()
                  (let ((head (car lst)))
                    (if (= (remainder num 2)
                           (remainder head 2))
                        (cons head (same-parity-iter num (cdr lst)))
                        (same-parity-iter num (cdr lst))))))))
    (cons num (same-parity-iter num lst))))

; ex 2.21
(define (square num)
  (* num num))

(define (square-list-1 items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

; ex 2.23
(define (my-for-each func lst)
  (if (not (null? lst))
      (begin (func (car lst))
             (my-for-each func (cdr lst)))))

; ex 2.24
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; ex 2.27
(define (deep-reverse sexp)
  (cond ((null? sexp) '())
        ((pair? (car sexp))
         (append (deep-reverse (cdr sexp))
                 (list (deep-reverse (car sexp)))))
        (else (append (deep-reverse (cdr sexp))
                      (list (car sexp))))))

; ex 2.28
(define (fringe sexp)
  (cond ((null? sexp) '())
        ((not (pair? sexp)) (list sexp))
        (else (append (fringe (car sexp))
                      (fringe (cdr sexp))))))

; ex 2.29
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (structure-is-weight? structure)
  (atom? structure))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (structure-is-weight? structure)
        structure
        (total-weight structure))))

(define (branch-force-moment branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (branch-balanced? branch)
  (let ((structure (branch-structure branch)))
    (or (structure-is-weight? #t)
        (mobile-balanced? structure))))

(define (mobile-balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (branch-balanced? left)
         (branch-balanced? right)
         (= (branch-force-moment left)
            (branch-force-moment right)))))

; ex 2.30
(define (square-tree-1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (square sub-tree)))
       tree))

; ex 2.31
(define (tree-map-1 func tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (func tree))
        (else (cons (tree-map-1 func (car tree))
                    (tree-map-1 func (cdr tree))))))

(define (tree-map-2 func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map-2 func sub-tree)
             (func sub-tree)))
       tree))

; ex 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((subsets-of-tail (subsets (cdr s)))
            (head (car s)))
        (append subsets-of-tail
                (map (lambda (subset)
                       (cons head subset))
                     subsets-of-tail)))))

; ex 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

; ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

; ex 2.35
(define (another-count-leaves t)
  (accumulate (lambda (left right-value)
                (+ (if (not (pair? left))
                       1
                       (another-count-leaves left))
                   right-value))
              0
              t))

; ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))