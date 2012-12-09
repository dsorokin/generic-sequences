;;;; GENERIC-SEQUENCES-STREAM -- Lazy streams for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(in-package :generic-seq-stream)

(defstruct stream-seq
  "Lazy stream"
  enum)

(defmethod seqp ((seq stream-seq))
  t)

(defmethod seq-enum ((seq stream-seq))
  (stream-seq-enum seq))

(defparameter *stream-lock* (make-lock "STREAM"))

(defun memo (delayed-exp thread-safe)
  (let ((x-defined nil)
        (x nil))
    (lambda ()
      (cond
       (x-defined x)
       ((not thread-safe)
        (let ((x-next (funcall delayed-exp)))
          (setf x x-next
                x-defined t)
          x-next))
       (t
        (with-lock-held (*stream-lock*)
          (if x-defined
              x
            (let ((x-next (funcall delayed-exp)))
              (setf x x-next
                    x-defined t)
              x-next))))))))

(defun seq->stream (seq &key (thread-safe nil))
  "Convert the sequence to a lazy stream that must always return the same elements."
  (make-stream-seq
   :enum (labels ((traverse (enum)
                    (if (null enum)
                        nil
                      (cons
                       (enum-car enum)
                       (memo (lambda () (traverse (enum-cdr enum)))
                             thread-safe)))))
           (traverse (seq-enum seq)))))
