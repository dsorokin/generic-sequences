;;;; GENERIC-SEQUENCES-TEST -- Tests for the generic sequences 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defpackage :generic-seq-test
  (:use :cl :iter :cont 
        :generic-seq
        :generic-seq-iter
        :generic-seq-cont
        :generic-seq-stream
        #+sbcl :sb-rt #-sbcl :rtest)
  (:nicknames #:gen-seq-test))
