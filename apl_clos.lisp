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
	(make-instance type :values (make-array size) :rank rank :shape shape :size size))

(defun array-to-list (array)
	(let ((lst '()))
		(dotimes (n (length array))
			(setf lst (append lst (list (aref array n)))))
	lst))

(defun tensor-fill (tensor scalar)
	(let ((tensor-scalar (tensor-construct-simple 'tensor (tensor-rank tensor) (tensor-shape tensor) (tensor-size tensor)))
		(dotimes (n (tensor-size tensor))
			(setf (aref (tensor-values tensor-scalar) n) (aref (tensor-values scalar))))
		tensor-scalar)))

(defun tensor-copy-simple (tensor)
	(let ((type 'tensor))
		(if (eq (tensor-rank tensor) 0)		
			(setf type 'scalar))
		(make-instance type :values (make-array (tensor-shape tensor)) :rank (tensor-rank tensor) :shape (tensor-shape tensor) :size (tensor-size tensor))))

(defun s (arg)
	(tensor-construct 'scalar (make-array 1 :initial-contents (list arg)) 0 '() 1))

(defun v (&rest args)
 	(let* ((s (list-length args))
 		   (shape (list s)))			
 		(tensor-construct 'tensor (make-array shape :initial-contents args) 1 shape s)))

(defmethod print-object ((object tensor) stream)
	(labels (
		(tensor-print-tester (indexes shape)
			(if (eq indexes nil)
				nil
				(if (not (eq (- (car (last shape)) 1) (car (last indexes))))
					t
					nil)))
		(tensor-print-new-lines (dimension indexes shape)
			(if (not (eq (tensor-rank object) dimension))
				(if (eq (tensor-print-tester indexes shape) t)
					(dotimes (d dimension)
						(format stream "~%")))))
		(tensor-print (stream object position shape indexes)
			(let ((cur-dimension (list-length shape))
				  (cur-dimension-value (first shape)))
				(if (eq shape nil)
					(progn 
						(format stream "~a" (aref (tensor-values object) position))
						(if (eq (tensor-print-tester indexes (tensor-shape object)) t)
							(format stream " "))
						(incf position))
					(progn
						(dotimes (dim cur-dimension-value)
							(setf position (tensor-print stream object position (cdr shape) (append indexes (list dim)))))
						(tensor-print-new-lines cur-dimension indexes shape))))
			position))
	(tensor-print stream object 0 (tensor-shape object) '())))

(defun tensor-apply (function &rest tensors)
	(let ((tensors-values (mapcar #'(lambda (n) (tensor-values n)) tensors))
		  (result (tensor-copy-simple (first tensors))))
		(apply #'map-into (tensor-values result) function tensors-values)
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
	(let* ((shape (array-to-list (tensor-values tensor1)))
		  (size-tensor1 (reduce #'* shape))
		  (result-tensor (tensor-construct-simple 'tensor (length shape) shape size-tensor1))
		  (tensor2-size (tensor-size tensor2)))
		(dotimes (position size-tensor1)
			(setf (aref (tensor-values result-tensor) position) (aref (tensor-values tensor2) (rem position tensor2-size))))
	result-tensor))

(defun catenate (tensor1 tensor2))

(defun member? (tensor1 tensor2))

(defun select (tensor1 tensor2))

;;;;;;;;;;;;;;;; MONADIC OPERATORS ;;;;;;;;;;;;;;;

(defun fold (function)
	#'(lambda (tensor) 
		(let ((result 0))
			(setf result (reduce function (tensor-values tensor)))
		(s result))))

(defun scan (function)
	#'(lambda (tensor)
		(let ((tensor-list (array-to-list (tensor-values tensor)))
			  (combination-list '())
			  (result-tensor (tensor-construct-simple 'tensor (tensor-size tensor) (tensor-shape tensor) (tensor-size tensor)))
			  (position 0))
			(progn
				(nreverse tensor-list)
				(mapl #'(lambda (sub-list) (push sub-list combination-list)) tensor-list)
				(dolist (combination combination-list)
					(progn
						(setf (aref (tensor-values result-tensor) position) (reduce function combination))
						(incf position))))
		result-tensor)))

(defun outer-product (function))

;;;;;;;;;;;;;;;; DYADIC OPERATORS  ;;;;;;;;;;;;;;;

(defun inner-product (function1 function2))

;;;;;;;;;;;;;;;;;;;; EXERCISES ;;;;;;;;;;;;;;;;;;;

(defun tally (tensor)
	(funcall (fold #'*) (shape tensor)))

(defun rank (tensor))

(defun within (tensor scalar1 scalar2))

(defun ravel (tensor) 
	(reshape (tally tensor) tensor))

(defun primes (scalar))
