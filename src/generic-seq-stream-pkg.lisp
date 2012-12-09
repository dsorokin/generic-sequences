;;;; GENERIC-SEQUENCES-STREAM -- Lazy streams for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defpackage :generic-seq-stream
  (:use :cl :generic-seq :bordeaux-threads)
  (:nicknames #:gen-seq-stream)
  (:export #:seq->stream))
