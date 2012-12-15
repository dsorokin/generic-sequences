;;;; GENERIC-SEQUENCES-ITERATE -- Generic sequence iteration for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(in-package :generic-seq-iter)

(defmacro define-generic-seq-iter-driver (name next-enum-operation traverse-init-value)
`(defmacro-driver (FOR var ,NAME seq)
   (let ((enum (gensym "ENUM"))
	 (enum-defined (gensym "ENUM-DEFINED"))
	 (enum-cdr (gensym "ENUM-CDR"))
	 (next-enum (gensym "NEXT-ENUM"))
	 (kwd (if generate 'generate 'for)))
     (labels ((traverse (var enum &optional enum-defined)
		(when var
		  (if (listp var)
		      (append (traverse (car var)
					(if enum-defined
					  `(enum-car ,enum)
					  `(enum-car (seq-enum ,enum))))
			      (traverse (cdr var)
					(if enum-defined
					  `(enum-cdr ,enum)
					  `(enum-cdr (seq-enum ,enum)))
					t))
		      `((for ,var next ,enum))))))
       `(progn
	  (with ,enum-defined = nil)
	  (generate ,enum next (let ((,enum-cdr 
				       (if ,enum-defined
					   (enum-cdr ,enum)
					   (progn
					     (setf ,enum-defined t)
					     (seq-enum ,seq)))))
				 (if ,enum-cdr 
				     ,enum-cdr 
				     (terminate))))
	  (,kwd ,next-enum next (,',next-enum-operation (next ,enum)))
	,@(traverse var next-enum ,traverse-init-value))))))

(define-generic-seq-iter-driver IN-SEQ enum-car nil)
(define-generic-seq-iter-driver ON-SEQ identity t)
