; ex 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (> d 0)
        (cons (/ n g) (/ d g))
        (cons (/ (* -1 n) g) (/ (* -1 d) g)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newlline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; ex 2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
                    (x-point (end-segment segment)))
                 2)
              (/ (+ (y-point (start-segment segment))
                    (y-point (end-segment segment)))
                 2)))

; ex 2.3
(define (make-rect-1 p1 p2)
  (let ((left-bottom
         (make-point (min (x-point p1)
                          (x-point p2))
                     (min (y-point p1)
                          (y-point p2))))
        (right-top
         (make-point (max (x-point p1)
                          (x-point p2))
                     (max (y-point p1)
                          (y-point p2)))))
    (cons left-bottom right-top)))

(define (width-rect-1 rect)
  (- (x-point (cdr rect))
     (x-point (car rect))))

(define (height-rect-1 rect)
  (- (y-point (cdr rect))
     (y-point (car rect))))

(define (perimeter-rect-1 rect)
  (* 2 (+ (width-rect-1 rect)
          (height-rect-1 rect))))

(define (area-rect-1 rect)
  (* (width-rect-1 rect)
     (height-rect-1 rect)))

(define (make-rect-2 p width height)
  (cons p (cons width height)))

(define (width-rect-2 rect)
  (car (cdr rect)))

(define (height-rect-2 rect)
  (cdr (cdr rect)))

(define (perimeter-rect-2 rect)
  (* 2 (+ (width-rect-2 rect)
          (height-rect-2 rect))))

(define (area-rect-2 rect)
  (* (width-rect-2 rect)
     (height-rect-2 rect)))

; ex 2.4
(define (another-cons x y)
  (lambda (m) (m x y)))

(define (another-car z)
  (z (lambda (p q) p)))

(define (another-cdr z)
  (z (lambda (p q) q)))

; ex 2.5
(define (cons-1 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-1 c)
  (if (= (remainder c 2) 0)
      (+ 1 (car-1 (/ c 2)))
      0))

(define (cdr-1 c)
  (if (= (remainder c 3) 0)
      (+ 1 (cdr-1 (/ c 3)))
      0))

; ex 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

; ex 2.7
(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((lower-bound-x (lower-bound x))
        (upper-bound-x (upper-bound x))
        (lower-bound-y (lower-bound y))
        (upper-bound-y (upper-bound y)))
    (cond ((>= lower-bound-x 0)
           (cond ((>= lower-bound-y 0)
                  (make-interval (* lower-bound-x lower-bound-y)
                                 (* upper-bound-x upper-bound-y)))
                 ((>= upper-bound-y 0)
                  (make-interval (* upper-bound-x lower-bound-y)
                                 (* upper-bound-x upper-bound-y)))
                 (else
                  (make-interval (* upper-bound-x lower-bound-y)
                                 (* lower-bound-x upper-bound-y)))))
          ((>= upper-bound-x 0)
           (cond ((>= lower-bound-y 0)
                  (make-interval (* lower-bound-x upper-bound-y)
                                 (* upper-bound-x upper-bound-y)))
                 ((>= upper-bound-y 0)
                  (let ((p1 (* lower-bound-x lower-bound-y))
                        (p2 (* lower-bound-x upper-bound-y))
                        (p3 (* upper-bound-x lower-bound-y))
                        (p4 (* upper-bound-x upper-bound-y)))
                    (make-interval (min p1 p2 p3 p4)
                                   (max p1 p2 p3 p4))))
                 (else
                  (make-interval (* upper-bound-x lower-bound-y)
                                 (* lower-bound-x lower-bound-y)))))
          (else
           (cond ((>= lower-bound-y 0)
                  (make-interval (* lower-bound-x upper-bound-y)
                                 (* upper-bound-x lower-bound-y)))
                 ((>= upper-bound-y 0)
                  (make-interval (* lower-bound-x upper-bound-y)
                                 (* lower-bound-x lower-bound-y)))
                 (else
                  (make-interval (* upper-bound-x upper-bound-y)
                                 (* lower-bound-x lower-bound-y))))))))
                  

(define (div-interval x y)
  (let ((upper-bound-y (upper-bound y))
        (lower-bound-y (lower-bound y)))
    (if (<= (* upper-bound-y lower-bound-y) 0)
        (begin (display "Error!")
               (newline))
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (width-interval interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))

; ex 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-center-width c w)))

(define (percent i)
  (* 100 (/ (width i) (abs (center i)))))