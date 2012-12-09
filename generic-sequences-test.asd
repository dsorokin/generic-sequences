;;;; GENERIC-SEQUENCES-TEST -- Tests for the generic sequences 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(defsystem generic-sequences-test
  :description "Unit tests for the generic sequences."
  :depends-on (generic-sequences
               generic-sequences-iterate
               generic-sequences-cont
               generic-sequences-stream
               #+sbcl sb-rt #-sbcl rt)
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "generic-seq-test-pkg")
     (:file "generic-seq-test")
     (:file "generic-seq-rt-pkg")
     (:file "generic-seq-rt")))))

(defmethod perform ((o test-op) (c (eql (find-system :generic-sequences-test))))
  (funcall (intern (string '#:run) '#:generic-seq-rt)))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :generic-sequences-test))))
  nil)
