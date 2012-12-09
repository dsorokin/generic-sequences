;;;; GENERIC-SEQUENCES-ITERATE -- Generic sequence iteration for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defpackage :generic-seq-iter
  (:use :cl :generic-seq :iter)
  (:nicknames #:gen-seq-iter)
  (:export #:in-seq))
