(define-class (complex real-part imag-part)
  (method (magnitude)
	  (sqrt (+ (* real-part real-part)
		   (* imag-part imag-part))))
  (method (angle)
	  (atan (/ imag-part real-part))) )

(define-class (counter)
  (instance-vars (count 0))
  (method (next)
	  (set! count (+ count 1))))
		 
