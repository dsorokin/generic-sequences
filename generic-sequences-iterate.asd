;;;; GENERIC-SEQUENCES-ITERATE -- Generic sequence iteration for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defsystem :generic-sequences-iterate
  :version "0.1"
  :description "Iteration of generic sequences for Common Lisp."
  :author "David Sorokin"
  :licence "MIT"
  :depends-on (:generic-sequences
               :iterate)
  :components ((:module "src"
                :serial t
                :components ((:file "generic-seq-iter-pkg")
                             (:file "generic-seq-iter")
                             (:static-file "README")
                             (:static-file "LICENSE")))))
