; ex 1.9
; 递归计算过程
(define (add-rec a b)
  (if (= a 0) b (+ (add-rec (- a 1) b) 1)))
	
; (add-rec 4 5)
; (+ 1 (add-rec 3 5))
; (+ 1 (+ 1 (add-rec 2 5)))
; (+ 1 (+ 1 (+ 1 (add-rec 1 5))))
; (+ 1 (+ 1 (+ 1 (+ 1 (add-rec 0 5)))))
; (+ 1 (+ 1 (+ 1 (+ 1 5))))
; (+ 1 (+ 1 (+ 1 6)))
; (+ 1 (+ 1 7))
; (+ 1 8)
; 9

; 迭代计算过程
(define (add-iter a b)
  (if (= a 0) b (add-iter (- a 1) (+ b 1))))

; (add-iter 4 5)
; (add-iter 3 6)
; (add-iter 2 7)
; (add-iter 1 8)
; (add-iter 0 9)
; 9


; ex 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
; (A 1 10)
; (A 0 (A 1 9))
; (* 2 (A 1 9))
; (* 2 (A 0 (A 1 8)))
; (* 2 (* 2 (A 1 8)))
; (* 2 (* 2 ... 10 times
; So, (A 1 10) = 2^10 = 1024

; (A 2 4)
; (A 1 (A 2 3))
; we saw that (A 1 x) is 2^x
; let's see what's (A 2 3)
; (A 1 (A 2 2)
; (A 1 (A 1 (A 2 1)))
; (A 1 (A 1 2)) => 2^(2^2) => 16
; So, (A 2 4) = 2^16 = 65536

; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 4)
; from above, this is 65536
; So, (A 3 3) = 2^16 = 65536

(define (f n) (A 0 n))
; 2n

(define (g n) (A 1 n))
; 2^n

(define (h n) (A 2 n))
; 2^2^... (n times)
