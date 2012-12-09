;;;; GENERIC-SEQUENCES-ITERATE -- Generic sequence iteration for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(in-package :generic-seq-iter)

(defmacro-driver (FOR var IN-SEQ seq)
  "All items of the sequence."
  (let ((enum (gensym "ENUM"))
        (enum-defined (gensym "ENUM-DEFINED"))
        (enum-cdr (gensym "ENUM-CDR"))
        (kwd (if generate 'generate 'for)))
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
       (,kwd ,var next (enum-car (next ,enum))))))
