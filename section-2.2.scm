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