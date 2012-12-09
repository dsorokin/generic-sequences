;;;; GENERIC-SEQUENCES-CONT -- Generic sequence comprehension for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defpackage :generic-seq-cont
  (:use :cl :generic-seq :cl-cont)
  (:nicknames #:gen-seq-cont)
  (:export
   #:with-enum/cc
   #:with-seq/cc
   #:yield/cc
   #:yield-enum/cc
   #:yield-seq/cc))
