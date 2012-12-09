;;;; GENERIC-SEQUENCES-TEST -- Tests for the generic sequences 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defpackage :generic-seq-rt
  (:use :cl #+sbcl :sb-rt #-sbcl :rtest)
  (:nicknames #:gen-seq-rt)
  (:import-from #+sbcl :sb-rt #-sbcl :rtest
                #:*compile-tests* #:*expected-failures*))
