;;;; GENERIC-SEQUENCES-CONT -- Generic sequence comprehension for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defsystem :generic-sequences-cont
  :version "0.1"
  :description "Generic sequence comprehension for Common Lisp."
  :author "David Sorokin"
  :licence "MIT"
  :depends-on (:generic-sequences
               :cl-cont)
  :components ((:module "src"
                :serial t
                :components ((:file "generic-seq-cont-pkg")
                             (:file "generic-seq-cont")
                             (:static-file "README")
                             (:static-file "LICENSE")))))
