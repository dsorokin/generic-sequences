;;;; GENERIC-SEQUENCES-TEST -- Tests for the generic sequences 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(in-package :generic-seq-test)

(deftest test-seqp-list
    (seqp '(1 2 3))
  t)

(deftest test-seqp-vector
    (seqp #(1 2 3))
  t)

(deftest test-seq->list
    (seq->list #(1 2 3))
  (1 2 3))

(deftest test-seq->vector
    (seq->vector '(1 2 3))
  #(1 2 3))

(deftest test-seq-null
    (seq-null '(1 2 3))
  nil)

(deftest test-seq-null-2
    (seq-null nil)
  t)

(deftest test-seq-null-3
    (seq-null #())
  t)

(deftest test-seq-car
    (seq-car #(1 2 3))
  1)

(deftest test-seq-cdr
    (seq->list (seq-cdr #(1 2 3)))
  (2 3))

(deftest test-seq-cons
    (seq->list (seq-cons 1 #(2 3)))
  (1 2 3))

(deftest test-seq-append
    (seq->list (seq-append '(0 1) #(2 3)))
  (0 1 2 3))

(deftest test-seq-equal
    (seq-equal '(1 2 3) #(1 2 3))
  t)

(deftest test-seq-compare
    (seq-compare '(1 4 3) #(1 2 3)
                 (lambda (x y)
                   (cond
                     ((< x y) -1)
                     ((> x y) 1)
                     ((= x y) 0))))
  1)

(deftest test-seq-length
    (seq-length #(1 2 3))
  3)

(deftest test-seq-elt
    (seq-elt #(1 2 3) 1)
  2)

(deftest test-seq-remove
    (seq->list (seq-remove 2 #(1 2 3)))
  (1 3))

(deftest test-seq-remove-if
    (seq->list (seq-remove-if #'zerop #(1 0 2 0 3)))
  (1 2 3))

(deftest test-seq-remove-if-not
    (seq->list (seq-remove-if-not #'zerop #(1 0 2 0 3)))
  (0 0))

(deftest test-seq-map
    (seq->list (seq-map (lambda (x) (list x :a)) #(1 2 3)))
  ((1 :a) (2 :a) (3 :a)))

(deftest test-seq-mappend
    (seq->list (seq-mappend (lambda (x) (list x :a)) #(1 2 3)))
  (1 :a 2 :a 3 :a))

(deftest test-seq-reduce
    (seq-reduce #'+ #(1 2 3 4 5))
  15)

(deftest test-seq-zip
    (seq->list (seq-zip #(:a :b :c) '(1 2 3)))
  ((:a 1) (:b 2) (:c 3)))

(deftest test-seq-take
    (seq->list (seq-take 3 #(1 2 3 4 5 6 7 8 9 0)))
  (1 2 3))

(deftest test-seq-take-while
    (seq->list (seq-take-while #'oddp #(1 3 2 4 5)))
  (1 3))

(deftest test-seq-take-while-not
    (seq->list (seq-take-while-not #'evenp #(1 3 2 4 5)))
  (1 3))

(deftest test-seq-take-nth
    (seq->list (seq-take-nth 2 #(1 2 3 4)))
  (1 3))

(deftest test-seq-drop
    (seq->list (seq-drop 2 #(1 2 3 4)))
  (3 4))

(deftest test-seq-drop-while
    (seq->list (seq-drop-while #'oddp #(1 3 2 4 5)))
  (2 4 5))

(deftest test-seq-drop-while-not
    (seq->list (seq-drop-while-not #'evenp #(1 3 2 4 5)))
  (2 4 5))

(deftest test-seq-split
    (destructuring-bind (x y)
        (seq-split 2 #(1 2 3 4))
      (list (seq->list x)
            (seq->list y)))
  ((1 2) (3 4)))

(deftest test-seq-split-if
    (destructuring-bind (x y)
        (seq-split-if (lambda (x) (>= x 3))  
                      #(1 2 3 4))
      (list (seq->list x)
            (seq->list y)))
  ((1 2) (3 4)))

(deftest test-seq-split-if-not
    (destructuring-bind (x y)
        (seq-split-if-not (lambda (x) (< x 3))  
                          #(1 2 3 4))
      (list (seq->list x)
            (seq->list y)))
  ((1 2) (3 4)))

(deftest test-seq-count
    (seq-count 1 #(1 2 -1 3 1 4 8 9 0) :key #'abs)
  3)

(deftest test-seq-count-if
    (seq-count-if (lambda (x) (= x 1)) #(1 2 -1 3 1 4 8 9 0) :key #'abs)
  3)

(deftest test-seq-count-if-not
    (seq-count-if-not (lambda (x) (/= x 1)) #(1 2 -1 3 1 4 8 9 0) :key #'abs)
  3)

(deftest test-seq-member
    (seq->list (seq-member 3 #(1 2 3 4)))
  (3 4))

(deftest test-seq-member-if
    (seq->list (seq-member-if (lambda (x) (= x 3)) #( 1 2 3 4)))
  (3 4))

(deftest test-seq-member-if-not
    (seq->list (seq-member-if-not (lambda (x) (< x 3)) #( 1 2 3 4)))
  (3 4))

(deftest test-seq-find
    (seq-find 5 #(1 2 3 4) :key #'1+)
  4)

(deftest test-seq-find-if
    (seq-find-if #'evenp #(1 2 3 4))
  2)

(deftest test-seq-find-if-not
    (seq-find-if-not #'oddp #(1 2 3 4))
  2)

(deftest test-seq-position
    (seq-position 3 #(1 2 3 4))
  2)

(deftest test-seq-positon-if
    (seq-position-if (lambda (x) (= x 3)) #(1 2 3 4))
  2)

(deftest test-seq-positon-if-not
    (seq-position-if-not (lambda (x) (< x 3)) #(1 2 3 4))
  2)

(deftest test-seq-every
    (seq-every #'characterp "abc")
  t)

(deftest test-seq-some
    (seq-some #'= '(1 2 3 4 5) #(5 4 3 2 1))
  t)

(deftest test-seq-notany
    (seq-notany #'> '(1 2 3 4) #(5 6 7 8) '(9 10 11 12))
  t)

(deftest test-seq-notevery
    (seq-notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12))
  nil)

(deftest test-seq-repeatedly
    (seq->list (seq-take 3 (seq-repeatedly (lambda () "hello"))))
  ("hello" "hello" "hello"))

(deftest test-seq-iterate
    (seq->list (seq-take 5 (seq-iterate #'1+ 5)))
  (5 6 7 8 9))

(deftest test-seq-repeat
    (seq->list (seq-take 5 (seq-repeat :a)))
  (:a :a :a :a :a))

(deftest test-seq-range
    (seq->list (seq-take 5 (seq-range)))
  (0 1 2 3 4))

(deftest test-seq-cycle
    (seq->list (seq-take 5 (seq-cycle '(:a :b :c))))
  (:a :b :c :a :b))

(deftest test-seq-interpose
    (seq->list (seq-interpose :a '(1 2 3 4)))
  (1 :a 2 :a 3 :a 4))

(deftest test-in-seq
    (seq->list
     (iter (for n from 1 to 5)
           (for x in-seq (seq-range))
           (collect x)))
  (0 1 2 3 4))

(deftest test-with-seq/cc
    (seq->list
     (with-seq/cc
      (yield/cc 0)
      (yield-seq/cc '(1 2 3))
      (yield/cc 4)))
  (0 1 2 3 4))

(deftest test-seq->stream
    (let* ((x (seq-take 10 (seq-repeatedly (lambda () (random 10)))))
           (y (seq->stream x)))
      (equal (seq->list y)
             (seq->list y)))
  t)
