(defclass tensor ()
 	((values
 		:initarg :values
 		:reader tensor-values)
 	 (rank
 	 	:initarg :rank
 	 	:reader tensor-rank)
 	 (shape
 	 	:initarg :shape
 	 	:reader tensor-shape)))

(defclass scalar (tensor) ())

(defun s (arg)
	(make-instance 'scalar :values (make-array nil :initial-contents arg)
				   		           :rank 0
				                 :shape '()))

(defun v (&rest args)
 	(let ((shape (list (list-length args))))
 		(make-instance 'tensor :values (make-array shape :initial-contents args)
 					 		             :rank 1
 					 		             :shape shape)))


;(defmethod print-object ((object tensor) stream)
;	(tensor-print stream object (tensor-shape object) '()))

;(defun tensor-print (stream tensor shape indexes)
;	(let ((cur-dimension (list-length shape)))
;    (if (eq shape nil)
;  		(progn
;        (format stream "~a" (apply #'aref (tensor-values tensor) indexes))
;        (if (not (eq (car (last indexes)) (- (car (last (tensor-shape tensor))) 1)))
;          (format stream " ")))
;  		(progn (dotimes (dim (car shape))
;  			   		(tensor-print stream tensor (cdr shape) (append indexes (list dim))))
;  				(if (not (eq (tensor-rank tensor) cur-dimension))
;  					(if (not (eq (car (last indexes)) (- (car (last shape)) 1)))
;  						(dotimes (r cur-dimension)
;  							(format stream "~%"))))))))

(defun tensor-scalar? (tensor)
  (if (eq (tensor-rank tensor) 0)
      T
      nil))

(defun tensor-apply-monadic (tensor new-tensor function shape indexes)
  (if (eq shape nil)
      (setf (apply #'aref (tensor-values new-tensor) indexes) (funcall function (apply #'aref (tensor-values tensor) indexes)))
      (dotimes (dim (car shape))
        (tensor-apply-monadic tensor new-tensor function (cdr shape) (append indexes (list dim)))))
  new-tensor)

(defgeneric tensor-apply-dyadic (tensor1 tensor2 new-tensor function shape indexes))

(defmethod tensor-apply-dyadic ((tensor1 tensor) (tensor2 tensor) new-tensor function shape indexes)
  (assert (and (eq (tensor-rank tensor1) (tensor-rank tensor2))
               (equal (tensor-shape tensor1) (tensor-shape tensor2)))
               (tensor1 tensor2)
               "ERROR: Argument tensors do not have the same size and shape.")
  (if (eq shape nil)
      (setf (apply #'aref (tensor-values new-tensor) indexes) (funcall function (apply #'aref (tensor-values tensor1) indexes) (apply #'aref (tensor-values tensor2) indexes)))
      (dotimes (dim (car shape))
        (tensor-apply-dyadic tensor1 tensor2 new-tensor function (cdr shape) (append indexes (list dim)))))
  new-tensor)

(defmethod tensor-apply-dyadic ((tensor1 scalar) (tensor2 tensor) new-tensor function shape indexes)
  (if (eq shape nil)
      (setf (apply #'aref (tensor-values new-tensor) indexes) (funcall function (apply #'aref (tensor-values tensor1) '()) (apply #'aref (tensor-values tensor2) indexes)))
      (dotimes (dim (car shape))
        (tensor-apply-dyadic tensor1 tensor2 new-tensor function (cdr shape) (append indexes (list dim)))))
  new-tensor)

(defmethod tensor-apply-dyadic ((tensor1 tensor) (tensor2 scalar) new-tensor function shape indexes)
  (if (eq shape nil)
      (setf (apply #'aref (tensor-values new-tensor) indexes) (funcall function (apply #'aref (tensor-values tensor1) indexes) (apply #'aref (tensor-values tensor2) '())))
      (dotimes (dim (car shape))
        (tensor-apply-dyadic tensor1 tensor2 new-tensor function (cdr shape) (append indexes (list dim)))))
  new-tensor)

;(defgeneric .- (tensor1 &optional tensor2))


;(defmethod .- (tensor1 &optional tensor2)
;    se for com 1 chama sym
;    se for com 2 chama sub)

;(defgeneric ./ (tensor1 &optional tensor2))

;(defmethod ./ (tensor1 &optional tensor2)
;    se for com 1 chama inverse
;    se for com 2 chama division)


(defun .! (arg))

(defun .sin (arg))

(defun .cos (arg))

(defun .not (arg))

(defun shape (arg))

(defun interval (arg))

(defun reshape (tensor1 tensor2))
