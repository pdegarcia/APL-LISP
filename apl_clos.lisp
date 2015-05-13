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

(defun tensor-copy-simple (tensor)
	(let ((type 'tensor))
		(if (eq (tensor-rank tensor) 0)		
			(setf type 'scalar))
		(make-instance type :values (make-array (tensor-size tensor)) :rank (tensor-rank tensor) :shape (tensor-shape tensor) :size (tensor-size tensor))))

(defun array-to-list (array)
	(let ((lst '()))
		(dotimes (n (length array))
			(setf lst (append lst (list (aref array n)))))
	lst))

(defun array-to-multi-list (array position shape)
	(let ((sub-list '())
	      (cur-dimension (list-length shape))§
	      (cur-dimension-value (first shape))
		  (result-pair nil))
		(if (eq cur-dimension 1)
			(dotimes (n cur-dimension-value)
				(progn 
					(setf sub-list (append sub-list (list (aref array position))))
					(incf position)))
			(dotimes (dim cur-dimension-value)
				(progn 
					(setf result-pair (array-to-multi-list array position (cdr shape)))
					(setf sub-list (append sub-list (list (car result-pair))))
					(setf position (cdr result-pair)))))
	(cons sub-list position)))

(defun multi-list-to-array (lst position shape array)
	(let ((cur-dimension (list-length shape)))
		(if (eq cur-dimension 1)
			(dolist (value lst)
				(progn 
					(setf (aref array position) value)
					(incf position)))
			(dolist (sub-list lst)
				(setf position (multi-list-to-array sub-list position (cdr shape) array))))
	position))

(defun tensor-fill (tensor scalar)
	(let ((tensor-scalar (tensor-construct-simple 'tensor (tensor-rank tensor) (tensor-shape tensor) (tensor-size tensor))))
		(dotimes (n (tensor-size tensor))
			(setf (aref (tensor-values tensor-scalar) n) (aref (tensor-values scalar) 0)))
		tensor-scalar))

(defun tensor-type-convert (tensor)
	(let ((result-tensor (tensor-copy-simple tensor)))
		(dotimes (position (tensor-size tensor))
			(if (eq (aref (tensor-values tensor) position) nil)
				(setf (aref (tensor-values result-tensor) position) 0)
				(setf (aref (tensor-values result-tensor) position) 1)))
	result-tensor))

(defun s (arg)
	(tensor-construct 'scalar (make-array 1 :initial-contents (list arg)) 0 '() 1))

(defun v (&rest args)
 	(let* ((values (if (listp (first args)) (first args) args))
 		   (s (list-length values))
 		   (shape (list s)))			
 		(tensor-construct 'tensor (make-array shape :initial-contents values) 1 shape s)))

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
	(v (tensor-shape tensor)))

(defun interval (value)
	(let ((interval-lst '()))
		(progn 
			(dotimes (n value)
				(setf interval-lst (append interval-lst (list (+ n 1)))))
			(v interval-lst))))

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
	(tensor-type-convert (tensor-apply-dyadic #'< tensor1 tensor2)))

(defun .> (tensor1 tensor2)
	(tensor-type-convert (tensor-apply-dyadic #'> tensor1 tensor2)))

(defun .<= (tensor1 tensor2)
	(tensor-type-convert (tensor-apply-dyadic #'<= tensor1 tensor2)))

(defun .>= (tensor1 tensor2)
	(tensor-type-convert (tensor-apply-dyadic #'>= tensor1 tensor2)))

(defun .= (tensor1 tensor2)
	(tensor-type-convert (tensor-apply-dyadic #'= tensor1 tensor2)))

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

(defun change-shape (shape value)
	(let ((new-shape '()))
		(progn 	
			(dotimes (n (- (list-length shape) 1))
				(setf new-shape (append new-shape (list (nth n shape)))))
			(setf new-shape (append new-shape (list value))))
	new-shape))

; adicionar assert que verifica tamanho do 1º tensor
(defun select (tensor1 tensor2)
	(labels (
		(select-recursive (lst shape array-last-dim)
			(let ((new-list '()))
				(if (eq (list-length shape) 1)
					(dotimes (p (length array-last-dim))
						(if (eq (aref array-last-dim p) 1)
							(setf new-list (append new-list (list (nth p lst))))))
					(dolist (sub-list lst)
						(setf new-list (append new-list (list (select-recursive sub-list (cdr shape) array-last-dim))))))
			new-list))
		(apply-select (tensor1 tensor2)
			(let* ((new-shape (change-shape (tensor-shape tensor2) (reduce #'+ (tensor-values tensor1))))
				   (result-tensor (tensor-construct-simple 'tensor (tensor-rank tensor2) new-shape (reduce #'* new-shape)))
				   (tensor2-list (car (array-to-multi-list (tensor-values tensor2) 0 (tensor-shape tensor2)))))
				(multi-list-to-array (select-recursive tensor2-list (tensor-shape tensor2) (tensor-values tensor1)) 0 new-shape (tensor-values result-tensor))
			result-tensor)))
	(apply-select tensor1 tensor2)))

(defun calc-shape (shape1 shape2)
	(let ((new-shape shape1))
		(dotimes (n (list-length shape2))
			(if (eq (nth n shape1) nil)
				(setf new-shape (append new-shape (list 0)))))
	(mapcar #'- shape2 (mapcar #'abs new-shape))))

;verificar n-1 dims = 0
(defun drop (tensor1 tensor2)
	(labels (
		(drop-recursive (lst depth dropped-values)
			(let ((new-list '()))
				(if (eq depth 0)
					(if (> dropped-values 0)
						(dotimes (n dropped-values)
							(setf new-list (cdr lst)))
						(if (< dropped-values 0)
							(dotimes (n (- (list-length lst) (abs dropped-values)))
								(setf new-list (append new-list (list (nth n lst)))))
							(if (eq dropped-values 0)
								(setf new-list (append new-list lst)))))
					(dolist (sub-list lst)
						(setf new-list (append new-list (list (drop-recursive sub-list (- depth 1) dropped-values))))))
			new-list))
		(apply-drop (tensor1 tensor2)
			(let* ((lst (car (array-to-multi-list (tensor-values tensor2) 0 (tensor-shape tensor2))))
				   (dimensions (tensor-values tensor1))
				   (depth 0)
				   (new-shape (calc-shape (array-to-list dimensions) (tensor-shape tensor2)))
				   (result-tensor (tensor-construct-simple 'tensor (list-length new-shape) new-shape (reduce #'* new-shape))))
				(progn 
					(dotimes (position (length dimensions))
						(progn
							(setf lst (drop-recursive lst depth (aref dimensions position)))
							(incf depth)))
					(multi-list-to-array lst 0 new-shape (tensor-values result-tensor)))
			result-tensor)))
	(apply-drop tensor1 tensor2)))

(defun catenate-auxiliar (tensor source-shape)
	(let ((new-shape '())
		  (sub-shape (subseq source-shape 0 (- (list-length source-shape) 1))))
		(progn
			(if (typep tensor 'scalar)
				(setf new-shape (append new-shape sub-shape)))
			(setf new-shape (append new-shape (list 1)))
			(reshape (v new-shape) tensor))))

(defun list-recursion (lst elements shape)
	(let ((new-list '()))	
		(if (eq (list-length shape) 1)
			(setf new-list (append (append new-list lst) elements))
			(dolist (sub-list lst)
				(setf new-list (append new-list (list-recursion sub-list elements (cdr shape))))))
	new-list))

(defgeneric catenate (tensor1 tensor2))

(defmethod catenate ((tensor1 scalar) (tensor2 scalar))
	(v (aref (tensor-values tensor1) 0) (aref (tensor-values tensor2) 0)))

(defmethod catenate ((tensor1 tensor) (tensor2 tensor))
	(assert (<= (abs (- (tensor-rank tensor1) (tensor-rank tensor2))) 1)
			(tensor1 tensor2)
			"ERROR: The difference between both tensor ranks is superior to one")
	(print "ole")
	)

(defmethod catenate((tensor1 scalar) (tensor2 tensor))
	(let ((new-tensor (catenate-auxiliar tensor1) (tensor-shape tensor2))
		  (new-tensor-list (array-to-multi-list (tensor-values new-tensor) 0 (tensor-shape new-tensor))))
		 (list-to-array (list-recursion new-tensor-list (list (tensor-values tensor1) (tensor-shape new-tensor)))		
	)))

(defmethod catenate((tensor1 tensor) (tensor2 scalar))
	(let ((new-tensor (tensor-construct-simple 'tensor (tensor-rank tensor1) shape (reduce #'* shape)))))
		(progn
			(setf (nth (- (tensor-rank tensor1) 1) (tensor-shape tensor1)) (+ (nth (- (tensor-rank tensor1) 1) (tensor-shape tensor1)) 1))
			(new-tensor-list (array-to-multi-list (tensor-values tensor1) 0 (tensor-shape tensor1)))))
			

		;(new-tensor-list (tensor-values new-tensor))))
	;new-tensor))

(defun member? (tensor1 tensor2)
	(let ((result-tensor (tensor-copy-simple tensor1)))
		(dotimes (position (tensor-size tensor1))
				(if (> (funcall fold #'+ (.= (s (aref tensor1 position)) tensor2)) 0)
					(setf (aref (tensor-values result-tensor) position) 1)
					(setf (aref (tensor-values result-tensor) position) 0)))
	result-tensor))

;;;;;;;;;;;;;;;; MONADIC OPERATORS ;;;;;;;;;;;;;;;

(defun fold (function)
	#'(lambda (tensor)
		(s (reduce function (tensor-values tensor)))))

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

(defun outer-product (function)
	#'(lambda (tensor1 tensor2)
		(let* ((shape (append (tensor-shape tensor1) (tensor-shape tensor2)))
			   (result-tensor (tensor-construct-simple 'tensor (list-length shape) shape (reduce #'* shape)))
			   (position 0))
			(dotimes (i (tensor-size tensor1))
				(dotimes (j (tensor-size tensor2))
					(progn
						(setf (aref (tensor-values result-tensor) position) (apply function (list (aref (tensor-values tensor1) i) (aref (tensor-values tensor2) j))))
						(incf position))))
		result-tensor)))

;;;;;;;;;;;;;;;; DYADIC OPERATORS  ;;;;;;;;;;;;;;;

(defun inner-product (function1 function2)
	#'(lambda (tensor1 tensor2)

	))

;;;;;;;;;;;;;;;;;;;; EXERCISES ;;;;;;;;;;;;;;;;;;;

(defun tally (tensor)
	(funcall (fold #'*) (shape tensor)))

(defun rank (tensor)
	(funcall (fold #'+ ) (.> (shape tensor) (s 0))))

(defun within (tensor scalar1 scalar2)
	(select (.* (.>= tensor scalar1) (.<= tensor scalar2)) tensor))

(defun ravel (tensor) 
	(reshape (tally tensor) tensor))

(defun primes (scalar))
