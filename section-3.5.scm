(#%require (only racket/base random))

(define the-empty-stream '())

(define stream-null? null?)

(define-syntax cons-stream
  (syntax-rules ()
    ((_ first second) (cons first (delay second)))))

(define (stream-ref s n)
  (if (= n 0)
     (stream-car s)
     (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
     the-empty-stream
     (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
     'done
     (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
     the-empty-stream
     (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
       ((pred (stream-car stream))
        (cons-stream (stream-car stream)
                    (stream-filter pred
                                  (stream-cdr stream))))
       (else (stream-filter pred (stream-cdr stream)))))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
         (begin (set! result (proc))
               (set! already-run? #t)
               result)
         result))))

; ex 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
     the-empty-stream
     (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
            (cons proc (map stream-cdr argstreams))))))

;
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
              (cons-stream 1
                          (add-streams (stream-cdr fibs)
                                      fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 2))))

(define (square n)
  (* n n))

(define (prime? n)
  (letrec ((iter
            (lambda (ps)
              (cond ((> (square (stream-car ps)) n) #t)
                   ((divisible? n (stream-car ps)) #f)
                   (else (iter (stream-cdr ps)))))))
    (iter primes)))

; ex 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1
              (mul-streams (integers-starting-from 2)
                          factorials)))

; ex 3.55
(define (partial-sums stream)
  (cons-stream (stream-car stream)
              (add-streams (stream-cdr stream)
                          (partial-sums stream))))

; ex 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
       ((stream-null? s2) s1)
       (else
        (let ((s1car (stream-car s1))
              (s2car (stream-car s2)))
          (cond ((< s1car s2car)
                 (cons-stream s1car (merge (stream-cdr s1) s2)))
               ((> s1car s2car)
                (cons-stream s2car (merge s1 (stream-cdr s2))))
               (else
                (cons-stream s1car
                            (merge (stream-cdr s1)
                                  (stream-cdr s2)))))))))

(define S (cons-stream 1
                      (merge (scale-stream S 2)
                            (merge (scale-stream S 3)
                                  (scale-stream S 5)))))

; ex 3.58
(define (expand num den radix)
  (cons-stream
   (quotinet (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; ex 3.59
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series stream)
  (div-streams stream integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream
                  (integrate-series sine-series)
                  -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; ex 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                 (stream-car s2))
              (add-streams (scale-stream (stream-cdr s2)
                                        (stream-car s1))
                          (mul-series (stream-cdr s1) s2))))

; ex 3.61
(define (invert-unit-series series)
  (cons-stream 1
              (scale-stream (mul-series (stream-cdr series)
                                       (invert-unit-series series))
                           -1)))

; ex 3.62
(define (div-series num denom)
  (let ((denom-const (stream-car denom)))
    (if (= denom-const 0)
       (error "denom constant term is zero" 'div-series)
       (mul-series
        num
        (scale-stream
         (invert-unit-series
          (scale-stream denom (/ 1 denom-const)))
         denom-const)))))

(define tangent-series
  (div-series sine-series cosine-series))

;
(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-stream x)
  (letrec ((guesses
            (cons-stream 1.0
                        (stream-map (lambda (guess)
                                      (sqrt-improve guess x))
                                   guesses))))
    guesses))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
              (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                         (+ s0 (* -2 s1) s2)))
                (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
              (make-tableau transform
                           (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
             (make-tableau transform s)))

; ex 3.64
(define (stream-limit stream tolerance)
  (letrec ((iter
            (lambda (head rest tolerance)
              (let ((cur (stream-car rest)))
                (if (< (abs (- cur head)) tolerance)
                   cur
                   (iter cur (stream-cdr rest) tolerance))))))
    (iter (stream-car stream) (stream-cdr stream) tolerance)))

(define (another-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; ex 3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
              (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define (display-n-stream-elements stream n)
  (if (> n 0)
     (if (stream-null? stream)
        'ok
        (begin (display (stream-car stream))
              (newline)
              (display-n-stream-elements (stream-cdr stream) (- n 1))))
     'ok))

;
(define (interleave s1 s2)
  (if (stream-null? s1)
     s2
     (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
               (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


; ex 3.67
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (interleave
    (stream-map
     (lambda (x) (list (stream-car s1) x))
     (stream-cdr s2))
    (interleave
     (stream-map
      (lambda (x) (list x (stream-car s2)))
      (stream-cdr s1))
     (all-pairs (stream-cdr s1) (stream-cdr s2))))))

; ???
(define (all-pairs-1 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
               (stream-cdr t))
    (pairs (stream-cdr s) t))))

; ex 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map
     (lambda (x) (cons (stream-car s) x))
     (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean
  (stream-filter
   (lambda (triplet)
     (= (+ (square (car triplet))
          (square (cadr triplet)))
       (square (caddr triplet))))
   (triples integers integers integers)))

; ex 3.70
(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
       ((stream-null? s2) s1)
       (else
        (let ((s1car (stream-car s1))
              (s2car (stream-car s2)))
          (cond ((<= (weight s1car) (weight s2car))
                 (cons-stream s1car
                             (merge-weighted weight (stream-cdr s1) s2)))
               (else
                (cons-stream s2car
                            (merge-weighted weight s1 (stream-cdr s2)))))))))

(define (weighted-pairs weight s1 s2)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    weight
    (stream-map (lambda (x) (list (stream-car s1) x))
               (stream-cdr s2))
    (weighted-pairs weight (stream-cdr s1) (stream-cdr s2)))))

(define sump
  (weighted-pairs (lambda (p) (+ (car p) (cadr p)))
                 integers
                 integers))

(define s235
  (let ((stream
         (stream-filter (lambda (number)
                          (or (= (remainder number 2) 0)
                             (= (remainder number 3) 0)
                             (= (remainder number 5) 0)))
                       integers)))
    (weighted-pairs
     (lambda (p)
       (let ((i (car p)) (j (cadr p)))
         (+ (* 2 i) (* 3 j) (* 5 i j))))
     stream
     stream)))

; ex 3.71
(define (cube-sum p)
  (let ((i (car p)) (j (cadr p)))
    (+ (* i i i) (* j j j))))

(define cubew
  (weighted-pairs cube-sum integers integers))

(define (ramanujan stream max-count)
  (if (> max-count 0)
     (let* ((head (stream-car stream))
            (currrent-cube-sum (cube-sum head))
            (rest (stream-cdr stream))
            (next (stream-car rest)))
       (if (= currrent-cube-sum (cube-sum next))
          (begin (display "(")
                (display currrent-cube-sum)
                (display " ")
                (display head)
                (display " ")
                (display next)
                (display ")")
                (newline)
                (ramanujan (stream-cdr stream) (- max-count 1)))
          (ramanujan (stream-cdr stream) max-count)))
     'ok))

; ex 3.72
(define (square-sum p)
  (+ (square (car p)) (square (cadr p))))

(define squarew
  (weighted-pairs
   square-sum
   integers
   integers))

(define (squares-3ways stream max-count)
  (if (> max-count 0)
     (let* ((head (stream-car stream))
            (currrent-square-sum (square-sum head))
            (rest (stream-cdr stream))
            (second (stream-car rest))
            (third (stream-car (stream-cdr rest))))
       (if (= currrent-square-sum
             (square-sum second)
             (square-sum third))
          (begin (display "(")
                (display currrent-square-sum)
                (display " ")
                (display head)
                (display " ")
                (display second)
                (display " ")
                (display third)
                (display ")")
                (newline)
                (squares-3ways (stream-cdr stream) (- max-count 1)))
          (squares-3ways (stream-cdr stream) max-count)))
     'ok))

;
(define (integral integrand initial-value dt)
  (let ((int
         (cons-stream initial-value
                     (add-streams (scale-stream integrand dt)))))
    int))

; ex 3.73
(define (RC R C dt)
  (let ((rc-model
         (lambda (v0 i-stream)
           (add-streams
            (scale-stream i-stream R)
            (integral
             (scale-stream i-stream (/ 1 C))
             v0
             dt)))))
    rc-model))

; ex 3.74
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                       (stream-car input-stream))))

;; (define zero-crossings (make-zero-crossings sense-data 0))

;; (define zero-crossings
;;   (stream-map sign-change-detector
;; 	      sense-data
;; 	      (cons-stream 0 sense-data)))

; ex 3.75
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                (make-zero-crossings (stream-cdr input-stream)
                                    (stream-car input-stream)
                                    avpt))))

; ex 3.76
(define (smooth s)
  (stream-map
   (lambda (x1 x2) (/ (+ x1 x2) 2))
   (cons-stream 0 s)
   x))

(define (make-zero-crossings input-stream transform last-value)
  (let ((transformed (transform input-stream)))
    (stream-map sign-change-detector
               transformed
               (cons-stream 0 transformed))))

;
(define (integral delayed-integrand initial-value dt)
  (letrec ((int
            (cons-stream initial-value
                        (let ((integrand (force delayed-integrand)))
                          (add-streams (scale-stream integrand dt)
                                      int)))))
    int))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; ex 3.77
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
              (let ((integrand (force delayed-integrand)))
                (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                            (+ (* dt (stream-car integrand))
                              initial-value)
                            dt)))))

; ex 3.78
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                          (scale-stream y b)))
  y)

; ex 3.79
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

; ex 3.80
(define (RLC R L C dt)
  (let ((rlc-model
         (lambda (vC0 iL0)
           (letrec ((iL (integral (delay diL) iL0 dt))
                    (vC (integral (delay dvC) vC0 dt))
                    (diL (add-streams (scale-stream vC (/ 1 L))
                                     (scale-stream iL (- (/ R L)))))
                    (dvC (scale-stream iL (/ -1 C))))
             (cons iL vC)))))
    rlc-model))

;
(define random-init
  120)

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define random-numbers
  (cons-stream random-init
              (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                       random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (let ((next
         (lambda (passed failed)
           (cons-stream
            (/ passed (+ passed failed))
            (monte-carlo
             (stream-cdr experiment-stream) passed failed)))))
    (if (stream-car experiment-stream)
       (next (+ passed 1) failed)
       (next passed (+ failed 1)))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
             (monte-carlo cesaro-stream 0 0)))

; ex 3.81
(define (random-number-generator random-init command-stream)
  (if (stream-null? command-stream)
     the-empty-stream
     (let ((command (stream-car command-stream)))
       (cond ((eq? command 'generator)
              (let ((random-number (random-update random-init)))
                (cons-stream random-number
                            (random-number-generator random-number
                                                    (stream-cdr command-stream)))))
            ((and (pair? command)
                 (eq? (car command) 'reset))
             (let ((reset-number (cdr command)))
               (cons-stream reset-number
                           (random-number-generator reset-number
                                                   (stream-cdr command-stream)))))
            (else
             (error "bad command -- " command))))))

; ex 3.82
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (random-number-pairs low1 high1 low2 high2)
  (cons-stream
   (cons (random-in-range low1 high1)
        (random-in-range low2 high2))
   (random-number-pairs low1 high1 low2 high2)))

(define (estimate-integral pred x1 x2 y1 y2)
  (let ((area (* (- x2 x1) (- y2 y1)))
        (randoms (random-number-pairs x1 x2 y1 y2)))
    (scale-stream (monte-carlo (stream-map pred randoms) 0 0) area)))

(define estimate-pi-stream
  (let ((unit-pred
         (lambda (p)
           (<= (+ (square (car p))
                 (square (cdr p))) 1))))
    (estimate-integral unit-pred -1.0 1.0 -1.0 1.0)))
