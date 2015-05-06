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

(defun s (arg)
	(make-instance 'tensor :values (make-array nil :initial-contents arg)
				   		   :rank 0
				           :shape '()))

   ; (defun v (&rest args)
   ; 	(let ((shape (list (list-length args))))
   ; 		(make-instance 'tensor :values (make-array shape :initial-contents args)
   ; 					 		   :rank 1
   ; 					 		   :shape shape)))

   (defun v (&rest args)
   	(let ((shape (list (list-length args))))
   		(make-instance 'tensor :values (make-array '(2 2 2 2) :initial-contents '((((1 1) (1 1)) ((1 1) (1 1))) (((1 1) (1 1)) ((1 1) (1 1)))))
   					 		   :rank 4
   					 		   :shape '(2 2 2 2))))

   ; (defun v (&rest args)
   ; 	(let ((shape (list (list-length args))))
   ; 		(make-instance 'tensor :values (make-array '(2 2) :initial-contents '((1 1) (1 1)))
   ; 					 		   :rank 2
   ; 					 		   :shape '(2 2))))

(defmethod print-object ((object tensor) stream)
	(tensor-print stream (tensor-values object) (tensor-rank object) (tensor-shape object) '()))

(defun tensor-print (stream values rank shape indexes)
	(if (eq shape nil)
		(format stream "~s " (apply #'aref values indexes))
		(progn (dotimes (dim (car shape))
			   		(tensor-print stream values rank (cdr shape) (append indexes (list dim))))
				(if (not (eq rank (list-length shape))) 
					(if (not (eql (car (last indexes)) (- (car (last shape)) 1)))
						(dotimes (r (list-length shape))
							(format stream "~%")))))))

(defun tensor-apply (values rank shape indexes))

(defun .- (arg ))

(defun ./ (arg))

(defun .! (arg))

(defun .sin (arg))

(defun .cos (arg))

(defun .not (arg))

(defun shape (arg))

(defun interval (arg))