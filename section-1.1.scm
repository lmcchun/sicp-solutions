; ex 1.4
; 对 if 表达式进行求值，得到的值应用到 a 和 b 上。

; ex 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))
; (test 0 (p))
; 应用序：test 是一个函数，将 test 作用于 0 和 (p)
; 时会对 0 和 (p) 求值，对 (p) 求值会造成死循环。
; 正则序：test 的参数会在需要时求值，对 if 表达式
; 求值时，由于 (= x 0) 为真，y 不会被求值。

; ex 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
; if 有特殊的求值规则
; 使用 new-if 会导致 then-clause 和 else-clause 均
; 被求值。在 sqrt-iter 中使用 new-if 会导致死循环。

; ex 1.7
; x 很小时（小于 0.001），
; (abs x (square guess)) 可能小于 0.001，
; 但 (/ (abs x (square guess)) x) 很大；
; x 很大时，由于计算机存储一个浮点数的空间固定，
; 浮点数越大，可以保存的精度就越低。
(define (sqrt-iter guess x)
  (let ((improved-guess (improve-square-root guess x)))
    (if (close-enough? guess improved-guess)
	improved-guess
	(sqrt-iter improved-guess x))))

(define (improve-square-root guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

; 猜测值越接近实际值时，前后两次的猜测值的差的绝对值
; 也越小；使用待开方值的数和猜测值的平方比较可能会导
; 致猜测值在正确值的两端来回变化。 
(define (close-enough? a b)
  (let ((ratio (/ a b)))
    (and (< ratio 1.001) (> ratio 0.999))))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

; ex 1.8
; 和 ex 1.7 类似
(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve-cube-root guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.00001))

(define (cube x)
  (* x x x))

(define (improve-cube-root guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root x)
  (cube-root-iter 1.0 x))
