 (defstruct (tensor (:print-object print-object-tensor))
 					values	
 					rank	
 					shape)

(defun s (arg)
	(make-tensor :values (make-array nil :initial-contents arg)
				 :rank 0
				 :shape 0))

(defun v (&rest args)
	(let ((shape (list (list-length args))))
		(make-tensor :values (make-array shape :initial-contents args)
					 :rank 1
					 :shape shape)))


(defun print-object-tensor ((object tensor) stream)
	(tensor-print stream (tensor-values object) (tensor-rank object) (tensor-shape object) '()))

(defun tensor-print (stream values rank shape indexes)
	(if (eq (list-length indexes) rank)
		(format stream "~s " (apply #'aref values indexes))
		(dotimes (dim (car shape))
			(tensor-print stream values rank (cdr shape) (append indexes (list dim))))))	