; ex 1.11
(define (F-recur n)
  (cond
    ((< n 3) n)
    (else
     (+ (F-recur (- n 1))
        (* 2 (F-recur (- n 2)))
        (* 3 (F-recur (- n 3)))))))
  
(define (F-iter-aux v1 v2 v3 n)
  (cond
   ((= n 0) v3)
   (else (F-iter-aux (+ v1 (* 2 v2) (* 3 v3)) v1 v2 (- n 1)))))

(define (F-iter n)
  (F-iter-aux 2 1 0 n))

; ex 1.12
(define (pascal-helper ln)
  (cond
    ((null? (cdr ln)) (list 1))
    (else (let ((ln1 (cdr ln)))
            (cons (+ (car ln) (car ln1))
                  (pascal-helper ln1))))))

(define (pascal n)
  (cond
    ((= n 1) (list (list 1)))
    (else (let ((ln1 (pascal (- n 1))))
            (cons (cons 1 (pascal-helper (car ln1))) ln1)))))

; 计算给定行、列的值
(define (pascal* row col)
  (cond ((= row 1) 1)
	((= row col) 1)
	((= col 1) 1)
	(else
	 (let ((prev-row (- row 1)))
	   (+ (pascal* prev-row (- col 1))
	      (pascal* prev-row col))))))
