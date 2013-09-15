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

; ex 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

; ex 2.38
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (letrec ((iter (lambda (result rest)
                   (if (null? rest)
                       result
                       (iter (op result (car rest))
                             (cdr rest))))))
    (iter initial sequence)))

; ex 2.39
(define (my-reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (my-reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

; ex 2.40
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (next-divisor test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (and (> n 1) (= n (smallest-divisor n))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cdr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cdr pair) (+ (car pair) (cdr pair))))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (cons i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; ex 2.41
(define (filter-triple n s)
  (filter
   (lambda (triple)
     (let ((i (car triple))
           (j (cadr triple))
           (k (caddr triple)))
       (and (= (+ i j k) s)
            (and (not (= i j))
                 (not (= j k))
                 (not (= k i))))))
   (let ((lst (enumerate-interval 1 n)))
     (flatmap
      (lambda (i)
        (flatmap
         (lambda (j)
           (map (lambda (k) (list i j k)) lst))
         lst))
      lst))))

; ex 2.42
(define empty-board '())

(define (make-position row column)
  (cons row column))

(define (adjoin-position row column positions)
  (cons (make-position row column) positions))

(define (position-row position)
  (car position))

(define (position-column position)
  (cdr position))

(define (safe? k positions)
  (if (null? positions)
      #t
      (let* ((recent-position (car positions))
             (recent-position-row (position-row recent-position))
             (recent-position-column (position-column recent-position)))
        (fold-left
         (lambda (result position)
           (let ((row (position-row position))
                 (column (position-column position)))
             (and result
                  (not (= recent-position-row row))
                  (not (= (+ recent-position-row recent-position-column)
                          (+ row column)))
                  (not (= (- recent-position-row recent-position-column)
                          (- row column))))))
         #t
         (cdr positions)))))

(define (queens board-size)
  (letrec ((queens-cols
            (lambda (k)
              (if (= k 0)
                  (list empty-board)
                  (filter
                   (lambda (positions) (safe? k positions))
                   (flatmap
                    (lambda (rest-of-queens)
                      (map (lambda (new-row)
                             (adjoin-position new-row k rest-of-queens))
                           (enumerate-interval 1 board-size)))
                    (queens-cols (- k 1))))))))
    (queens-cols board-size)))