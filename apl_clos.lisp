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
	"Function that constructs a tensor with the given arguments."
	(make-instance type :values values :rank rank :shape shape :size size))

(defun tensor-construct-simple (type rank shape size)
	"Function that constructs a tensor with the given arguments but not initializing the slot values."
	(make-instance type :values (make-array size) :rank rank :shape shape :size size))

(defun tensor-copy-simple (tensor)
	"Function that constructs a tensor from the slots of the given tensor but not initializing the slot values."
	(let ((type 'tensor))
		(if (eq (tensor-rank tensor) 0)		
			(setf type 'scalar))
		(make-instance type :values (make-array (tensor-size tensor)) :rank (tensor-rank tensor) :shape (tensor-shape tensor) :size (tensor-size tensor))))

(defun array-to-list (array)
	"Function that converts the given 1d array to a flat list."
	(let ((lst '()))
		(dotimes (n (length array))
			(setf lst (append lst (list (aref array n)))))
	lst))

(defun array-to-multi-list (array position shape)
	"Function that converts the given array to a nested list taking the given shape into account."
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
	"Function that converts the given nested list to a flat array."
	(let ((cur-dimension (list-length shape)))
		(if (eq cur-dimension 1)
			(dolist (value lst)
				(progn 
					(setf (aref array position) value)
					(incf position)))
			(dolist (sub-list lst)
				(setf position (multi-list-to-array sub-list position (cdr shape) array))))
	position))

(defun list-to-array (lst)
	"Function that converts the given list to an array."
	(make-array (list-length lst) :initial-contents lst))

(defun tensor-convert-to-int (tensor)
	"Function that converts the given boolean tensor to int representation."
	(let ((result-tensor (tensor-copy-simple tensor)))
		(dotimes (position (tensor-size tensor))
			(if (eq (aref (tensor-values tensor) position) nil)
				(setf (aref (tensor-values result-tensor) position) 0)
				(setf (aref (tensor-values result-tensor) position) 1)))
	result-tensor))

(defun tensor-convert-to-bool (tensor)
	"Function that converts the given int tensor to boolean."
	(let ((result-tensor (tensor-copy-simple tensor)))
		(dotimes (position (tensor-size tensor))
			(if (eq (aref (tensor-values tensor) position) 0)
				(setf (aref (tensor-values result-tensor) position) nil)
				(setf (aref (tensor-values result-tensor) position) t)))
	result-tensor))

(defun s (arg)
	"Function that constructs a scalar with the given argument."
	(tensor-construct 'scalar (make-array 1 :initial-contents (list arg)) 0 '() 1))

(defun v (&rest args)
	"Function that constructs a vector with the given arguments."
 	(let* ((values (if (listp (first args)) (first args) args))
 		   (s (list-length values))
 		   (shape (list s)))			
 		(tensor-construct 'tensor (make-array shape :initial-contents values) 1 shape s)))

(defmethod print-object ((object tensor) stream)
	"Specialization to tensors of the generic function Print-Object.
	 Prints the given tensors according to the rules of dimensions, etc."
	(labels (
		(can-print? (shape indexes)
			(if (null indexes)
				t
				(and (eql (- (car shape) 1) (car indexes)) 
					 (can-print? (cdr shape) (cdr indexes)))))
		(tensor-print (stream object position shape indexes)
			(let ((cur-dimension (list-length shape))
				  (cur-dimension-value (first shape)))
				(if (eq shape nil)
					(progn 
						(format stream "~a" (aref (tensor-values object) position))
						(unless (can-print? (last (tensor-shape object)) (last indexes))
							(format stream " "))
						(incf position))
					(progn
						(dotimes (dim cur-dimension-value)
							(setf position (tensor-print stream object position (cdr shape) (append indexes (list dim)))))
						(unless (can-print? (tensor-shape object) indexes)
							(format stream "~%"))))
			position)))
	(tensor-print stream object 0 (tensor-shape object) '())))

(defun tensor-apply (function &rest tensors)
	"Function that given one or two tensors of the same size, applies the given function to them."
	(let ((tensors-values (mapcar #'(lambda (n) (tensor-values n)) tensors))
		  (result (tensor-copy-simple (first tensors))))
		(apply #'map-into (tensor-values result) function tensors-values)
	result))

;;;;;;;;;;;;;;;; MONADIC FUNCTIONS ;;;;;;;;;;;;;;;;

(defgeneric .- (tensor1 &optional tensor2))

(defmethod .- (tensor1 &optional tensor2)
	"Specialization of the generic function .- that decides what function to call, taking into account
	 the number of given arguments."
	(if (eq tensor2 nil)
		(tensor-apply #'- tensor1)
		(tensor-apply-dyadic #'- tensor1 tensor2)))

(defgeneric ./ (tensor1 &optional tensor2))

(defmethod ./ (tensor1 &optional tensor2)
	"Specialization of the generic function ./ that decides what function to call, taking into account
	 the number of given arguments."
	(if (eq tensor2 nil)
		(tensor-apply #'(lambda (n) (/ 1 n)) tensor1)
		(tensor-apply-dyadic #'/ tensor1 tensor2)))

(defun .! (tensor)
	"Monadic function that given a tensor, returns a new tensor resulting from applying factorial to each of its elements."
	(tensor-apply #'! tensor))

(defun .sin (tensor)
	"Monadic function that given a tensor, returns a new tensor resulting from applying sin to each of its elements."
	(tensor-apply #'sin tensor))

(defun .cos (tensor)
	"Monadic function that given a tensor, returns a new tensor resulting from applying cosin to each of its elements."
	(tensor-apply #'cos tensor))

(defun .not (tensor)
	"Monadic function that given a tensor, returns a new tensor resulting from applying negation to each of its elements."
	(tensor-apply #'(lambda (n) (if (eq n 0) 1 0)) tensor))

(defun shape (tensor)
	"Monadic function that given a tensor, returns a vector containing each of its dimension values."
	(v (tensor-shape tensor)))

(defun interval (value)
	"Monadic function that given an integer, returns a new vector containing all the integer elements from zero up to the 
	 integer."
	(iota (s value)))

(defun iota (scalar)
	(let ((iota-lst '())) 
		(dotimes (n (tensor-values scalar))
			(setf iota-lst (append iota-lst (list (+ n 1)))))
	(v iota-lst)))

;;;;;;;;;;;;;;;; DYADIC FUNCTIONS ;;;;;;;;;;;;;;;;
(defgeneric tensor-apply-dyadic (function tensor1 tensor2))

(defmethod tensor-apply-dyadic (function (tensor1 tensor) (tensor2 tensor))
	"Specialization of the generic function tensor-apply-dyadic, that performs the intended actions only if the arguments 
	 are both tensors."
	(assert (and (eq (tensor-rank tensor1) (tensor-rank tensor2))
				 (equal (tensor-shape tensor1) (tensor-shape tensor2)))
			(tensor1 tensor2)
			"ERROR: The given tensors do not have the same rank nor shape")
	(tensor-apply function tensor1 tensor2))

(defmethod tensor-apply-dyadic (function (tensor1 scalar) (tensor2 tensor))
	"Specialization of the generic function tensor-apply-dyadic, that performs the intended actions only if the 1st argument 
	 is a scalar and the 2nd is a tensor."
	(tensor-apply function (reshape (shape tensor2) tensor1) tensor2))

(defmethod tensor-apply-dyadic (function (tensor1 tensor) (tensor2 scalar))
	"Specialization of the generic function tensor-apply-dyadic, that performs the intended actions only if the 1st argument 
	 is a tensor and the 2nd is a scalar."
	(tensor-apply function tensor1 (reshape (shape tensor1) tensor2)))

(defun .+ (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the sum to each of their elements."
	(tensor-apply-dyadic #'+ tensor1 tensor2))

(defun .* (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the multiplication to each of their elements."
	(tensor-apply-dyadic #'* tensor1 tensor2))

(defun .// (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the integer division to each of their elements."
	(tensor-apply-dyadic #'floor tensor1 tensor2))

(defun .% (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the remainder of integer division to each of their elements."
	(tensor-apply-dyadic #'rem tensor1 tensor2))

(defun .< (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the less than relation to each of their elements."
	(tensor-convert-to-int (tensor-apply-dyadic #'< tensor1 tensor2)))

(defun .> (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the greater than relation to each of their elements."
	(tensor-convert-to-int (tensor-apply-dyadic #'> tensor1 tensor2)))

(defun .<= (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the less than or equal relation to each of their elements."
	(tensor-convert-to-int (tensor-apply-dyadic #'<= tensor1 tensor2)))

(defun .>= (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the greater than or equal relation to each of their elements."
	(tensor-convert-to-int (tensor-apply-dyadic #'>= tensor1 tensor2)))

(defun .= (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the equal relation to each of their elements."
	(tensor-convert-to-int (tensor-apply-dyadic #'= tensor1 tensor2)))

(defun .or (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the logical disjunction to each of their elements."
	(tensor-convert-to-int (tensor-apply-dyadic #'(lambda (v1 v2) (or v1 v2)) (tensor-convert-to-bool tensor1) (tensor-convert-to-bool tensor2))))

(defun .and (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor resulting from applying the logical conjunction to each of their elements."
	(tensor-convert-to-int (tensor-apply-dyadic #'(lambda (v1 v2) (and v1 v2)) (tensor-convert-to-bool tensor1) (tensor-convert-to-bool tensor2))))

(defun reshape (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor whose shape is the one given in the 1st argument and contents are the ones from
	 the 2nd argument."
	(let* ((shape (array-to-list (tensor-values tensor1)))
		   (size-tensor1 (reduce #'* shape))
		   (result-tensor (tensor-construct-simple 'tensor (length shape) shape size-tensor1))
		   (tensor2-size (tensor-size tensor2)))
		(dotimes (position size-tensor1)
			(setf (aref (tensor-values result-tensor) position) (aref (tensor-values tensor2) (rem position tensor2-size))))
	result-tensor))

; adicionar assert que verifica tamanho do 1º tensor
(defun select (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor whose last dimension elements were selected according to the 1st argument."
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
			(let* ((new-shape (append (array-to-list (tensor-values (drop (s -1) (shape tensor2)))) (list (reduce #'+ (tensor-values tensor1)))))
				   (result-tensor (tensor-construct-simple 'tensor (tensor-rank tensor2) new-shape (reduce #'* new-shape)))
				   (tensor2-list (car (array-to-multi-list (tensor-values tensor2) 0 (tensor-shape tensor2)))))
				(multi-list-to-array (select-recursive tensor2-list (tensor-shape tensor2) (tensor-values tensor1)) 0 new-shape (tensor-values result-tensor))
			result-tensor)))
	(apply-select tensor1 tensor2)))

(defun calc-shape (shape1 shape2)
	"Auxiliar function to calculate the new shape of a dropped tensor."
	(let ((new-shape shape1))
		(dotimes (n (list-length shape2))
			(if (eq (nth n shape1) nil)
				(setf new-shape (append new-shape (list 0)))))
	(mapcar #'- shape2 (mapcar #'abs new-shape))))

(defun drop (tensor1 tensor2)
	"Dyadic function that given two tensors, returns a new tensor whose dimensions have n less first/last elements according to the 1st argument."
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

(defgeneric catenate (tensor1 tensor2))

(defmethod catenate ((tensor1 scalar) (tensor2 scalar))
	"Specialization of generic function catenate, that performs the intended actions only if the arguments are both scalars."
	(v (aref (tensor-values tensor1) 0) (aref (tensor-values tensor2) 0)))

(defmethod catenate ((tensor1 tensor) (tensor2 scalar))
	"Specialization of generic function catenate, that performs the intended actions only if the 1st argument is a tensor 
	and the 2nd argument is a scalar."
	(catenate tensor1 (reshape (v (append (array-to-list (tensor-values (drop (s -1) (shape tensor1)))) (list 1))) tensor2)))

(defmethod catenate ((tensor1 scalar) (tensor2 tensor))
	"Specialization of generic function catenate, that performs the intended actions only if the 1st argument is a scalar
	and the 2nd argument is a tensor."
	(catenate (reshape (v (append (array-to-list (tensor-values (drop (s -1) (shape tensor2)))) (list 1))) tensor1) tensor2))

(defmethod catenate ((tensor1 tensor) (tensor2 tensor))
	"Specialization of generic function catenate, that catenates two tensors along the last dimension only if they are both tensors
	and their n-1 dimensions are equal and their ranks differ at most by one."
	(labels (
		(catenate-calc-shape (tensor1 tensor2 smaller-tensor which-smaller)
			(let ((last-dimension 0))
				(cond	((eq which-smaller 0) 	(append (array-to-list (tensor-values (drop (s -1) (shape tensor1)))) (list (+ (car (last (tensor-shape tensor1))) (car (last (tensor-shape tensor2)))))))
					 	((eq which-smaller 1) 	(append (array-to-list (tensor-values (drop (s -1) (shape tensor2)))) (list (+ (car (last (tensor-shape smaller-tensor))) (car (last (tensor-shape tensor2)))))))
					  	(t 						(append (array-to-list (tensor-values (drop (s -1) (shape tensor1)))) (list (+ (car (last (tensor-shape tensor1))) (car (last (tensor-shape smaller-tensor))))))))))
		(catenate-recursive (lst1 lst2 shape)
			(let ((new-list '()))
				(if (eq (list-length shape) 1)
					(setf new-list (append new-list (append lst1 lst2)))
					(loop for sub-list1 in lst1
						  for sub-list2 in lst2
						  do (setf new-list (append new-list (list (catenate-recursive sub-list1 sub-list2 (cdr shape)))))))
			new-list))
		(apply-catenate (tensor1 tensor2)
			(let ((smaller-tensor nil)
				  (which-tensor-smaller? 0)
				  (need-resize? t)
				  (result-tensor nil)
				  (new-shape nil))
				(progn	(cond	((eq (- (tensor-rank tensor1) (tensor-rank tensor2)) 1) (setf smaller-tensor tensor2) (setf which-tensor-smaller? 2))
				  	      	 	((eq (- (tensor-rank tensor2) (tensor-rank tensor1)) 1) (setf smaller-tensor tensor1) (setf which-tensor-smaller? 1))
				          	 	((eq (- (tensor-rank tensor2) (tensor-rank tensor1)) 0) (setf need-resize? nil))
				          	 	(t (error "ERROR::CATENATE::RANK: The difference between the tensors' rank is greater than 1.")))
						(progn	(if (eq need-resize? t)
									(setf smaller-tensor (reshape (v (append (tensor-shape smaller-tensor) (list 1))) smaller-tensor)))
								(setf new-shape (catenate-calc-shape tensor1 tensor2 smaller-tensor which-tensor-smaller?))
								(setf result-tensor (tensor-construct-simple 'tensor (tensor-rank tensor1) new-shape (reduce #'* new-shape)))
								(cond	((eq which-tensor-smaller? 2) (multi-list-to-array (catenate-recursive (car (array-to-multi-list (tensor-values tensor1) 0 (tensor-shape tensor1))) (car (array-to-multi-list (tensor-values smaller-tensor) 0 (tensor-shape smaller-tensor))) new-shape) 0 new-shape (tensor-values result-tensor)))
								  		((eq which-tensor-smaller? 1) (multi-list-to-array (catenate-recursive (car (array-to-multi-list (tensor-values smaller-tensor) 0 (tensor-shape smaller-tensor))) (car (array-to-multi-list (tensor-values tensor2) 0 (tensor-shape tensor2))) new-shape) 0 new-shape (tensor-values result-tensor)))
								  		(t (multi-list-to-array (catenate-recursive (car (array-to-multi-list (tensor-values tensor1) 0 (tensor-shape tensor1))) (car (array-to-multi-list (tensor-values tensor2) 0 (tensor-shape tensor2))) new-shape) 0 new-shape (tensor-values result-tensor))))))
			result-tensor)))
	(apply-catenate tensor1 tensor2)))

 (defun member? (tensor1 tensor2)
 	"Dyadic function that given two tensors, returns a new tensor resulting from testing if each element of 1st tensor
 	 is present somewhere on the 2nd tensor."
 	(let ((result-tensor (tensor-copy-simple tensor1)))
 		(dotimes (position (tensor-size tensor1))
 				(if (> (reduce #'+ (tensor-values (.= (s (aref (tensor-values tensor1) position)) tensor2))) 0)
 					(setf (aref (tensor-values result-tensor) position) 1)
 					(setf (aref (tensor-values result-tensor) position) 0)))
 	result-tensor))

;;;;;;;;;;;;;;;; MONADIC OPERATORS ;;;;;;;;;;;;;;;

(defun fold (function)
	"Monadic operator that given a function, returns another function that applies the given one to successive elements of a given vector."
	#'(lambda (tensor)
		(reduce function (map 'array #'s (tensor-values tensor)))))

(defun scan (function)
	"Monadic operator that given a function, returns another function that applies the given one to increasingly larger subsets of the elements 
	 of the given vector."
	#'(lambda (tensor)
		(let ((result-list '())
			  (scalar-tensor (map 'array #'s (tensor-values tensor))))
			 (dotimes (position (tensor-size tensor))
			 	(setf result-list (append result-list (list (aref (tensor-values (reduce function scalar-tensor :start 0 :end (+ position 1))) 0)))))
		(v result-list))))

(defun outer-product (function)
	"Monadic operator that given a function, returns another function that applies the given one to all combinations of elements of the 
	 given tensors."
	#'(lambda (tensor1 tensor2)
		(let ((shape (append (tensor-shape tensor1) (tensor-shape tensor2)))
			  (scalar-tensor (tensor-construct 'tensor (map 'array #'s (tensor-values tensor1)) (tensor-rank tensor1) (tensor-shape tensor1) (tensor-size tensor1)))
			  (result-list '()))
			  (dotimes (position (tensor-size tensor1))
			  	(setf result-list (append result-list (array-to-list (tensor-values (apply function (list (aref (tensor-values scalar-tensor) position) tensor2)))))))
		(tensor-construct 'tensor (list-to-array result-list) (list-length shape) shape (reduce #'* shape)))))

;;;;;;;;;;;;;;;; DYADIC OPERATORS  ;;;;;;;;;;;;;;;

(defun inner-product (function1 function2)
	#'(lambda (tensor1 tensor2)))

;;;;;;;;;;;;;;;;;;;; EXERCISES ;;;;;;;;;;;;;;;;;;;

(defun tally (tensor)
	"Function that given a tensor, returns a scalar with the number of elements of the given tensor."
	(funcall (fold #'*) (shape tensor)))

(defun rank (tensor)
	"Function that given a tensor, returns a scalar with the number of dimensions of the given tensor."
	(funcall (fold #'+ ) (.>= (shape tensor) (s 0))))

(defun within (tensor scalar1 scalar2)
	"Function that given a tensor and two scalars, returns a vector containing only the elements of the given tensor that
	 are in the range between scalar1 and scalar2."
	(select (.* (.>= tensor scalar1) (.<= tensor scalar2)) tensor))

(defun ravel (tensor) 
	"Function that given a tensor, returns a vector containing all the elements of the given tensor."
	(reshape (tally tensor) tensor))

(defun primes (scalar)
	"Function that given a scalar, returns a vector containing all the prime elements from 2 up to the scalar, inclusive."
	(let ((droppped-vector (drop (s 1) (interval ))))
		 (select (.not (member? droppped-vector (funcall (outer-product #'.*) droppped-vector droppped-vector))) droppped-vector)))
