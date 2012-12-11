;;;; GENERIC-SEQUENCES -- Generic sequences for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defpackage :generic-seq
  (:use :cl)
  (:nicknames #:gen-seq)
  (:export
   #:enum-cons
   #:enum-car
   #:enum-cdr
   #:enum-append
   #:make-seq
   #:delay-seq
   #:seq
   #:seqp
   #:seq-enum
   #:seq->list
   #:seq->vector
   #:seq-null
   #:seq-car
   #:seq-cdr
   #:seq-equal
   #:seq-compare
   #:seq-length
   #:seq-elt
   #:seq-cons
   #:seq-append
   #:seq-remove-if
   #:seq-remove-if-not
   #:seq-remove
   #:seq-map
   #:seq-mappend
   #:seq-reduce
   #:seq-zip
   #:seq-foreach
   #:seq-take
   #:seq-take-while
   #:seq-take-while-not
   #:seq-take-nth
   #:seq-drop
   #:seq-drop-while
   #:seq-drop-while-not
   #:seq-split
   #:seq-split-if
   #:seq-split-if-not
   #:seq-member
   #:seq-member-if
   #:seq-member-if-not
   #:seq-position
   #:seq-position-if
   #:seq-position-if-not
   #:seq-find
   #:seq-find-if
   #:seq-find-if-not
   #:seq-every
   #:seq-some
   #:seq-notany
   #:seq-notevery
   #:seq-repeatedly
   #:seq-iterate
   #:seq-repeat
   #:seq-range
   #:seq-cycle
   #:seq-interpose))
