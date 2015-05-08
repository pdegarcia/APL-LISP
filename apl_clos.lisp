(defclass tensor ()
 	((values
 		:initarg :values
 		:reader tensor-values)
 	 (rank
 	 	:initarg :rank
 	 	:reader tensor-rank)
 	 (shape
 	 	:initarg :shape
 	 	:reader tensor-shape)
 	 (size
 	 	:initarg :size
 	 	:reader tensor-size)))

(defclass scalar (tensor) ())

(defun tensor-construct (type values rank shape size)
	(make-instance type :values values :rank rank :shape shape :size size))

(defun tensor-copy-simple (tensor)
	(let ((type 'tensor))
		(if (eq (tensor-rank tensor) 0)		
			(setf type 'scalar))
		(make-instance type :values (make-array (tensor-shape tensor)) :rank (tensor-rank tensor) :shape (tensor-shape tensor) :size (tensor-size tensor))))

(defun s (arg)
	(tensor-construct 'scalar (make-array nil :initial-contents arg) 0 '() 1))

(defun v (&rest args)
 	(let* ((s (list-length args))
 		   (shape (list s)))			
 		(tensor-construct 'tensor (make-array shape :initial-contents args) 1 shape s)))


; (defmethod print-object ((object tensor) stream)
; 	(tensor-print stream object (tensor-shape object) '()))

; (defun tensor-print (stream tensor shape indexes)
; 	(let ((cur-dimension (list-length shape)))
;     (if (eq shape nil)
;   		(progn
;         (format stream "~a" (apply #'aref (tensor-values tensor) indexes))
;         (if (not (eq (car (last indexes)) (- (car (last (tensor-shape tensor))) 1)))
;           (format stream " ")))
;   		(progn (dotimes (dim (car shape))
;   			   		(tensor-print stream tensor (cdr shape) (append indexes (list dim))))
;   				(if (not (eq (tensor-rank tensor) cur-dimension))
;   					(if (not (eq (car (last indexes)) (- (car (last shape)) 1)))
;   						(dotimes (r cur-dimension)
;   							(format stream "~%"))))))))

(defun tensor-displace (tensor)
 	(make-array (tensor-size tensor) :displaced-to (tensor-values tensor)))

(defun tensor-apply (function &rest tensors)
	(let* ((displaced-values '())
		  (result (tensor-copy-simple (first tensors)))	
		  (result-displaced (tensor-displace result)))
		(progn 
			(dolist (tensor tensors) 
				(setf displaced-values (append displaced-values (list (tensor-displace tensor)))))
			(apply #'map-into result-displaced function displaced-values))
	result))

;;;;;;;;;;;;;;;; MONADIC FUNCTIONS ;;;;;;;;;;;;;;;;

;(defgeneric .- (tensor1 &optional tensor2))


;(defmethod .- (tensor1 &optional tensor2)
;    se for com 1 chama sym
;    se for com 2 chama sub)

;(defgeneric ./ (tensor1 &optional tensor2))

;(defmethod ./ (tensor1 &optional tensor2)
;    se for com 1 chama inverse
;    se for com 2 chama division)


(defun .! (tensor)
	(tensor-apply #'! tensor))

(defun .sin (tensor)
	(tensor-apply #'sin tensor))

(defun .cos (tensor)
	(tensor-apply #'cos tensor))

(defun .not (tensor))

(defun shape (tensor)
	(let ((shape-length (list-length (tensor-shape tensor))))
		(tensor-construct 'tensor (make-array shape-length :initial-contents (tensor-shape tensor)) 1 shape-length shape-length)))

(defun interval (value)
	(let ((interval-lst '()))
		(progn 
			(dotimes (n value)
				(setf interval-lst (append interval-lst (list (+ n 1)))))
			(tensor-construct 'tensor (make-array (list value) :initial-contents interval-lst) 1 (list value) value))))

;;;;;;;;;;;;;;;; DYADIC FUNCTIONS ;;;;;;;;;;;;;;;;

(defun drop (tensor1 tensor2))

(defun reshape (tensor1 tensor2))

(defun catenate (tensor1 tensor2))

(defun member? (tensor1 tensor2))

(defun select (tensor1 tensor2))

;;;;;;;;;;;;;;;; MONADIC OPERATORS ;;;;;;;;;;;;;;;

(defun fold (function))

(defun scan (function))

(defun outer-product (function))

;;;;;;;;;;;;;;;; DYADIC OPERATORS  ;;;;;;;;;;;;;;;

(defun inner-product (function1 function2))

;;;;;;;;;;;;;;;;;;;; EXERCISES ;;;;;;;;;;;;;;;;;;;

(defun tally (tensor))

(defun rank (tensor))

(defun within (tensor scalar1 scalar2))

(defun primes (scalar))
