; ex 3.28
(define (or-gate a1 a2 output)
  (let ((or-action-procedure
	 (lambda ()
	   (let ((new-value
		  (logical-or (get-signal a1) (get-signal a2))))
	     (after-delay or-gate-delay
			  (lambda ()
			    (set-signal! output new-value)))))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok))

; ex 3.29 ???
(define (or-gate-dm a1 a2 output)
  (let ((b1 (make-wire))
	(b2 (make-wire))
	(c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)
    'ok))

(define or-gate-dm-delay
  (+ inverter-delay
     and-gate-delay
     inverter-delay))

; ex 3.30
; TODO
