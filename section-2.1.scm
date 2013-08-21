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