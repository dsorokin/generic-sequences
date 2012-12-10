;;;;
;;;; The Fibonacci sequence
;;;;

(defpackage test-seq-fib 
  (:use :cl :iter 
        :gen-seq :gen-seq-cont 
        :gen-seq-iter :gen-seq-stream))

(in-package :test-seq-fib)

;;;
;;; Prerequisites
;;;

(defun memo (fun)
  (let ((already-run? nil)
        (result nil))
    #'(lambda ()
        (if (not already-run?)
            (progn
              (setf result (funcall fun))
              (setf already-run? t)
              result)
            result))))

(defmacro delay (exp)
  `(memo #'(lambda() ,exp)))

(defun force (delayed-exp)
  (funcall delayed-exp))

;;(defmacro letrec (((name value)) &body body)
;;  (let ((x (gensym)))
;;    `(let ((,x (cons nil nil)))
;;       (symbol-macrolet ((,name (force (car ,x))))
;;         (setf (car ,x) (delay ,value))
;;         ,@body))))

(defmacro letrec (decls &body body)
  (labels
      ((make-infos (decls)
         (loop for decl in decls collect
              (destructuring-bind (name value) decl
                (list :name name :value value :gen (gensym)))))
       (gen-let (info)
         `(,(getf info :gen) nil))
       (gen-symbol-macrolet (info)
         `(,(getf info :name) (force ,(getf info :gen))))
       (gen-setf (info)
         `(setf ,(getf info :gen) (delay ,(getf info :value))))
       (gen-lets (infos)
         (loop for info in infos collect (gen-let info)))
       (gen-symbol-macrolets (infos)
         (loop for info in infos collect (gen-symbol-macrolet info)))
       (gen-setfs (infos)
         (loop for info in infos collect (gen-setf info))))
    (let ((infos (make-infos decls)))
      `(let (,@(gen-lets infos))
         (symbol-macrolet (,@(gen-symbol-macrolets infos))
           ,@(gen-setfs infos)
           ,@body)))))

;;;
;;; Using Sequence Comprehension
;;;

(defparameter *fibs-1*
  (with-seq/cc
    (do ((a 1 b)
         (b 1 (+ a b)))
        (nil)
      (yield/cc a))))

;;;
;;; Using Lazy Stream
;;;

(defparameter *fibs-2*
  (letrec 
      ((fibs (seq->stream
              (seq-cons
               1 (seq-cons
                  1 (seq-map
                     (lambda (x)
                       (+ (first x) (second x)))
                     (seq-zip
                      (delay-seq fibs)
                      (delay-seq (seq-cdr fibs)))))))))
    fibs))

;;;
;;; Writing the enumerator (the most efficient ever possible here)
;;;

(defparameter *fibs-3*
  (make-seq
   (labels ((traverse (a b)
              (enum-cons
               a (traverse b (+ a b)))))
     (traverse 1 1))))

;;;
;;; Output Results
;;;

;; ? (seq->list (seq-take 20 *fibs-1*))
;; (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)

;; ? (seq->list (seq-take 20 *fibs-2*))
;; (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)

;; ? (seq->list (seq-take 20 *fibs-3*))
;; (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)

;; ? (iter (for i from 1 to 20)
;;         (for x in-seq *fibs-3*)
;;         (collect x))
;; (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)
