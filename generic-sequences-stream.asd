;;;; GENERIC-SEQUENCES-STREAM -- Lazy streams for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defsystem :generic-sequences-stream
  :version "0.1"
  :description "Lazy streams for Common Lisp."
  :author "David Sorokin"
  :licence "MIT"
  :depends-on (:generic-sequences
               :bordeaux-threads)
  :components ((:module "src"
                :serial t
                :components ((:file "generic-seq-stream-pkg")
                             (:file "generic-seq-stream")
                             (:static-file "README")
                             (:static-file "LICENSE")))))
