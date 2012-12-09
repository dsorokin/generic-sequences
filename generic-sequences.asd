;;;; GENERIC-SEQUENCES -- Generic sequences for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defsystem :generic-sequences
  :version "0.1"
  :description "Generic sequences for Common Lisp."
  :author "David Sorokin"
  :licence "MIT"
  :components ((:module "src"
                :serial t
                :components ((:file "generic-seq-pkg")
                             (:file "generic-seq")
                             (:static-file "README")
                             (:static-file "LICENSE")))))

(defmethod perform ((o test-op) (c (eql (find-system :generic-sequences))))
  (operate 'load-op :generic-sequences-test)
  (operate 'test-op :generic-sequences-test))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :generic-sequences))))
  nil)

