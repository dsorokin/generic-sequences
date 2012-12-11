;;;; GENERIC-SEQUENCES-CONT -- Generic sequence comprehension for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(in-package :generic-seq-cont)

(defmacro with-enum/cc (&body body)
  `(with-call/cc ,@body nil))

(defmacro with-seq/cc (&body body)
  `(make-seq (with-enum/cc ,@body)))

(defmacro yield/cc (item)
  (let ((k (gensym)))
    `(call/cc (lambda (,k) (enum-cons ,item (funcall ,k))))))

(defmacro yield-enum/cc (enum)
  (let ((k (gensym)))
    `(call/cc (lambda (,k) (enum-append ,enum (funcall ,k))))))

(defmacro yield-seq/cc (seq)
  `(yield-enum/cc (seq-enum ,seq)))
