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

(defun tensor-construct-simple (type rank shape size)
	(make-instance type :values (make-array shape) :rank rank :shape shape :size size))

(defun array-to-list (array)
	(let ((lst '()))
		(dotimes (n (length array))
			(setf lst (append lst (list (aref array n)))))
	lst))

(defun tensor-fill (tensor scalar)
	(let* ((tensor-scalar (tensor-construct-simple 'tensor (tensor-rank tensor) (tensor-shape tensor) (tensor-size tensor)))
		   (tensor-scalar-displaced (tensor-displace tensor-scalar)))
		(dotimes (n (tensor-size tensor))
			(setf (aref tensor-scalar-displaced n) (aref (tensor-values scalar))))
		tensor-scalar))

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

 ; (defmethod print-object ((object tensor) stream)
 ; 	(tensor-print stream object (tensor-shape object)))

 ; (defun tensor-print (stream tensor shape)
 ; 	(let ((cur-dimension (first shape)))
	;  	(if (eq shape nil)
	;  		(dotimes (position cur-dimension)
	;  			(progn 
	;  				(format stream "~a" (aref (tensor-values) position))
	;  				(if (not (eq position (- cur-dimension 1)))
	;  					(format stream " "))))
	;  		(tensor-print stream tensor (cdr shape)))))
 
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

(defgeneric .- (tensor1 &optional tensor2))

(defmethod .- (tensor1 &optional tensor2)
	(if (eq tensor2 nil)
		(tensor-apply #'- tensor1)
		(tensor-apply-dyadic #'- tensor1 tensor2)))

(defgeneric ./ (tensor1 &optional tensor2))

(defmethod ./ (tensor1 &optional tensor2)
	(if (eq tensor2 nil)
		(tensor-apply #'inverse tensor1)
		(tensor-apply-dyadic #'/ tensor1 tensor2)))

(defun inverse (value)
	(/ 1 value))

(defun .! (tensor)
	(tensor-apply #'! tensor))

(defun .sin (tensor)
	(tensor-apply #'sin tensor))

(defun .cos (tensor)
	(tensor-apply #'cos tensor))

(defun .not (tensor)
	(tensor-apply #'negation tensor))

(defun negation (value)
	(if (eq value 0)
		1
		0))

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
(defgeneric tensor-apply-dyadic (function tensor1 tensor2))

(defmethod tensor-apply-dyadic (function (tensor1 tensor) (tensor2 tensor))
	(assert (and (eq (tensor-rank tensor1) (tensor-rank tensor2))
				 (equal (tensor-shape tensor1) (tensor-shape tensor2)))
			(tensor1 tensor2)
			"ERROR: The given tensors do not have the same rank nor shape")
	(tensor-apply function tensor1 tensor2))

(defmethod tensor-apply-dyadic (function (tensor1 scalar) (tensor2 tensor))
	(let ((tensor-scalar (tensor-fill tensor2 tensor1)))
		(tensor-apply function tensor-scalar tensor2)))

(defmethod tensor-apply-dyadic (function (tensor1 tensor) (tensor2 scalar))
	(let ((tensor-scalar (tensor-fill tensor1 tensor2)))
		(tensor-apply function tensor1 tensor-scalar)))

(defun .+ (tensor1 tensor2)
	(tensor-apply-dyadic #'+ tensor1 tensor2))

(defun .* (tensor1 tensor2)
	(tensor-apply-dyadic #'* tensor1 tensor2))

(defun .// (tensor1 tensor2)
	(tensor-apply-dyadic #'floor tensor1 tensor2))

(defun .% (tensor1 tensor2)
	(tensor-apply-dyadic #'rem tensor1 tensor2))

(defun .< (tensor1 tensor2)
	(tensor-apply-dyadic #'< tensor1 tensor2))

(defun .> (tensor1 tensor2)
	(tensor-apply-dyadic #'> tensor1 tensor2))

(defun .<= (tensor1 tensor2)
	(tensor-apply-dyadic #'<= tensor1 tensor2))

(defun .>= (tensor1 tensor2)
	(tensor-apply-dyadic #'>= tensor1 tensor2))

(defun .= (tensor1 tensor2)
	(tensor-apply-dyadic #'= tensor1 tensor2))

(defun .or (tensor1 tensor2)
	(tensor-apply-dyadic #'or tensor1 tensor2))

(defun .and (tensor1 tensor2)
	(tensor-apply-dyadic #'and tensor1 tensor2))

(defun drop (tensor1 tensor2))

; ver de escalares
(defun reshape (tensor1 tensor2)
	(let* ((shape (array-to-list (tensor-displace tensor1)))
		  (size-tensor1 (reduce #'* shape))
		  (displaced-tensor2 (tensor-displace tensor2))
		  (result-tensor (tensor-construct-simple 'tensor (list-length shape) shape size-tensor1))
		  (displaced-result-tensor (tensor-displace result-tensor)))
		(dotimes (position size-tensor1)
			(setf (aref displaced-result-tensor position) (aref displaced-tensor2 (rem position (length displaced-tensor2)))))
	result-tensor))

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

(defun tally (tensor)
	(s (tensor-size tensor)))

(defun rank (tensor)
	(s (tensor-rank tensor)))

(defun within (tensor scalar1 scalar2))

(defun ravel (tensor)
	(let ((displaced-tensor (tensor-displace tensor)))
		(tensor-construct 'tensor displaced-tensor 1 (length displaced-tensor) (length displaced-tensor))))

(defun primes (scalar))
