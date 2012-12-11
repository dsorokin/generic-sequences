;;;; GENERIC-SEQUENCES -- Generic sequences for Common Lisp 
;;;; by David Sorokin <david.sorokin@gmail.com>, 2012
;;;;
;;;; Licensed under MIT. See LICENSE for details.

(in-package :generic-seq)

;;;
;;; Sequence and Enumerator
;;;
;;; Sequence has an enumerator. The enumerator is either NIL
;;; or a cons cell which CDR is a function that returns the 
;;; next enumerator or NIL.
;;;

(defgeneric seqp (seq)
  (:documentation "Test whether this is a sequence."))

(defgeneric seq-enum (seq)
  (:documentation "Returns either NIL or an enumerator."))

(defmethod seqp (seq)
  (declare (ignore seq))
  nil)

(deftype seq ()
  "The generic sequence type."
  `(satisfies seqp))

(defmacro enum-cons (item enum)
  "Construct a new enumerator."
  `(cons ,item (lambda () ,enum)))

(defmacro enum-car (enum)
  "Return the CAR part."
  `(car ,enum))

(defmacro enum-cdr (enum)
  "Return the CDR part."
  `(funcall (cdr ,enum)))

(defun enum-append-2 (enum-1 delayed-enum-2)
  "Append two enumerators."
  (if (null enum-1)
      (funcall delayed-enum-2)
      (enum-cons (enum-car enum-1)
                 (enum-append-2 (enum-cdr enum-1)
                                delayed-enum-2))))

(defmacro enum-append (&rest enums)
  "Append the specified enumerators."
  (reduce (lambda (enum-1 enum-2)
            `(enum-append-2 ,enum-1
                            (lambda () ,enum-2)))
          enums
          :from-end t))

;;;
;;; Basic sequence
;;;

(defstruct basic-seq
  "Represents the basic sequence that is defined only by its enumerator."
  delayed-enum)

(defmethod seqp ((seq basic-seq))
  t)

(defmethod seq-enum ((seq basic-seq))
  (funcall (basic-seq-delayed-enum seq)))

(defmacro make-seq (&body enum)
  "Create a sequence by specifying its enumerator."
  `(make-basic-seq
    :delayed-enum (lambda () ,@enum)))

;;;
;;; Delayed sequence
;;;

(defstruct delayed-seq
  "The delayed sequence."
  delayed-seq)

(defmethod seqp ((seq delayed-seq))
  t)

(defmethod seq-enum ((seq delayed-seq))
  (seq-enum (funcall (delayed-seq-delayed-seq seq))))

(defmacro delay-seq (seq)
  "Delay the sequence."
  `(make-delayed-seq
    :delayed-seq (lambda () ,seq)))

;;;
;;; Basic functions
;;;

(defun seq->list (seq)
  "Convert the sequence to a list."
  (typecase seq
    (list seq)
    (t (do ((enum (seq-enum seq) (enum-cdr enum))
            (list nil (push (enum-car enum) list)))
           ((null enum) (nreverse list))))))

(defun seq->vector (seq)
  "Convert the sequence to a vector."
  (typecase seq
    (vector seq)
    (t (do ((enum (seq-enum seq) (enum-cdr enum))
            (vect (make-array 4 :fill-pointer 0 :adjustable t)))
           ((null enum) vect)
         (vector-push-extend (enum-car enum) vect)))))

(defun seq-cons (item seq)
  "Construct a new sequence that begins with the specified item and ends with the sequence."
  (make-seq (enum-cons item (seq-enum seq))))

(defun seq-append (&rest seqs)
  "Append the specified sequences."
  (make-seq
    (labels ((traverse (delayed-enums)
               (if (null delayed-enums)
                   nil
                 (enum-append
                  (funcall (car delayed-enums))
                  (traverse (cdr delayed-enums))))))
      (traverse (loop for seq in seqs
                      collect (let ((x seq))  ;; a closure by value
                                (lambda () (seq-enum x))))))))

(defun seq-remove-if-not (test seq &key (key #'identity))
  "Like the REMOVE-IF-NOT function but applied to the generic sequence."
  (make-seq
    (labels ((traverse (enum)
               (cond
                ((null enum) nil)
                ((funcall test (funcall key (enum-car enum)))
                 (enum-cons
                  (enum-car enum)
                  (traverse (enum-cdr enum))))
                (t (traverse (enum-cdr enum))))))
      (traverse (seq-enum seq)))))

(defun seq-remove-if (test seq &key (key #'identity))
  "Like the REMOVE-IF function but applied to the generic sequence."
  (seq-remove-if-not (complement test) seq :key key))

(defun seq-remove (item seq &key (test #'eql) (key #'identity))
  "Like the REMOVE function but applied to the generic sequence."
  (seq-remove-if (lambda (x) (funcall test item x)) seq :key key))

(defun seq-map-1 (function seq)
  "Like the MAPCAR function but applied to a single generic sequence."
  (make-seq
    (labels ((traverse (enum)
               (if (null enum)
                   nil
                 (enum-cons
                  (funcall function (enum-car enum))
                  (traverse (enum-cdr enum))))))
      (traverse (seq-enum seq)))))

(defun seq-map (function &rest seqs)
  "Like the MAPCAR function but applied to generic sequences."
  (assert (not (null seqs)))
  (if (null (cdr seqs))
      (seq-map-1 function (car seqs))
    (seq-map-1 (lambda (xs) (apply function xs))
               (apply #'seq-zip seqs))))

(defun seq-mappend-1 (function seq)
  "Like SEQ-MAP-1 but the specified function must return a sequence. 
In other words, this is a monadic bind function."
  (make-seq
    (labels ((traverse (enum)
               (if (null enum)
                   nil
                 (labels ((traverse-2 (enum-2)
                            (if (null enum-2)
                                (traverse (enum-cdr enum))
                              (enum-cons
                               (enum-car enum-2)
                               (traverse-2 (enum-cdr enum-2))))))
                   (traverse-2 (seq-enum (funcall function (enum-car enum))))))))
      (traverse (seq-enum seq)))))

(defun seq-mappend (function &rest seqs)
  "Like SEQ-MAP but the specified function must return a sequence."
  (assert (not (null seqs)))
  (if (null (cdr seqs))
      (seq-mappend-1 function (car seqs))
    (seq-mappend-1 (lambda (xs) (apply function xs))
                   (apply #'seq-zip seqs))))

(defun seq-zip (&rest seqs)
  "Return a sequence that returns lists of items from the provided sequences."
  (make-seq
    (labels ((traverse (enums)
               (if (loop for enum in enums
                         thereis (null enum))
                   nil
                 (enum-cons
                  (loop for enum in enums
                        collect (enum-car enum))
                  (traverse (loop for enum in enums
                                  collect (enum-cdr enum)))))))
      (traverse (loop for seq in seqs
                      collect (seq-enum seq))))))

(defun seq-foreach-1 (function seq)
  "Apply the specified function to the sequence."
  (do ((enum (seq-enum seq) (enum-cdr enum)))
      ((null enum) (values))
    (funcall function (enum-car enum))))

(defun seq-foreach (function &rest seqs)
  "Apply the specified function to the sequences."
  (assert (not (null seqs)))
  (if (null (cdr seqs))
      (seq-foreach-1 function (car seqs))
    (seq-foreach-1 (lambda (xs) (apply function xs))
                   (apply #'seq-zip seqs))))

(defun seq-length (seq)
  "Return the length of the specified sequence."
  (do ((enum (seq-enum seq) (enum-cdr enum))
       (len 0 (1+ len)))
      ((null enum) len)))

(defun seq-elt (seq index)
  "Access the element of the sequence specified by index."
  (do ((enum (seq-enum seq) (enum-cdr enum))
       (len 0 (1+ len)))
      ((cond
        ((null enum) 
         (error "The index ~D is out or range for sequence ~S" index seq))
        ((= len index) 
         (return (enum-car enum)))
        (t nil)))))

(defun seq-equal (seq-1 seq-2 &key (test #'eql) (key #'identity))
  "Test two sequences for equality."
  (do ((enum-1 (seq-enum seq-1) (enum-cdr enum-1))
       (enum-2 (seq-enum seq-2) (enum-cdr enum-2)))
      ((cond
        ((null enum-1)
         (return (null enum-2)))
        ((null enum-2)
         (return nil))
        ((not (funcall test 
                       (funcall key (enum-car enum-1))
                       (funcall key (enum-car enum-2))))
         (return nil))
        (t nil)))))

(defun seq-compare (seq-1 seq-2 test &key (key #'identity))
  "Compare two sequences where the test function for items must return <0, >0 or 0."
  (do ((enum-1 (seq-enum seq-1) (enum-cdr enum-1))
       (enum-2 (seq-enum seq-2) (enum-cdr enum-2)))
      ((cond
        ((null enum-1)
         (return (if (null enum-2) 0 -1)))
        ((null enum-2)
         (return 1))
        ((let ((x (funcall test 
                           (funcall key (enum-car enum-1))
                           (funcall key (enum-car enum-2)))))
           (if (zerop x) 
               nil
             (return x))))
        (t nil)))))

(defun seq-take (n seq)
  "Take the first N elements of the sequence."
  (make-seq
    (labels ((traverse (enum acc)
               (cond 
                ((null enum) nil)
                ((zerop acc) nil)
                (t (enum-cons
                    (enum-car enum)
                    (traverse (enum-cdr enum)
                              (1- acc)))))))
      (traverse (seq-enum seq) n))))

(defun seq-take-while (predicate seq &key (key #'identity))
  "SEQ-TAKE-WHILE takes a predicate function taking a single argument 
and a sequence. It returns a sequence of all items in the original sequence, up until 
the first item for which the predicate function returns NIL."
  (make-seq
    (labels ((traverse (enum)
               (cond
                ((null enum) nil)
                ((not (funcall predicate (funcall key (enum-car enum)))) nil)
                (t (enum-cons
                    (enum-car enum)
                    (traverse (enum-cdr enum)))))))
      (traverse (seq-enum seq)))))

(defun seq-take-while-not (predicate seq &key (key #'identity))
  "SEQ-TAKE-WHILE-NOT takes two arguments, a predicate function taking a single argument 
and a sequence. It returns a sequence of all items in the original sequence, up until 
the first item for which the predicate function returns T."
  (seq-take-while (complement predicate) seq :key key))

(defun seq-take-nth (n seq)
  "TAKE-NTH takes two arguments, a number and a sequence. It returns a sequence of 
items from the supplied sequence, taking the first item and every Nth item, where 
N is the supplied number."
  (make-seq
    (labels ((traverse (enum)
               (if (null enum)
                   nil
                 (enum-cons 
                  (enum-car enum)
                  (do ((enum enum (enum-cdr enum))
                       (acc n (1- acc)))
                      ((cond
                        ((null enum) (return nil))
                        ((zerop n) (return nil))
                        ((zerop acc) (return (traverse enum)))
                        (t nil))))))))
      (traverse (seq-enum seq)))))

(defun seq-drop (n seq)
  "Drop the first N elements of the sequence and return the rest."
  (do ((enum (seq-enum seq) (enum-cdr enum))
       (acc n (1- acc)))
      ((cond
        ((null enum) (return nil))
        ((zerop acc) (return (make-seq enum)))
        (t nil)))))

(defun seq-drop-while (predicate seq &key (key #'identity))
  "SEQ-DROP-WHILE takes a predicate function taking a single argument 
and a sequence. It returns a sequence of all items in the original sequence, starting 
from the first item for which the predicate function returns NIL."
  (do ((enum (seq-enum seq) (enum-cdr enum)))
      ((cond
        ((null enum) 
         (return nil))
        ((not (funcall predicate (funcall key (enum-car enum)))) 
         (return (make-seq enum)))
        (t nil)))))

(defun seq-drop-while-not (predicate seq &key (key #'identity))
  "SEQ-DROP-WHILE-NOT takes a predicate function taking a single argument 
and a sequence. It returns a sequence of all items in the original sequence, starting 
from the first item for which the predicate function returns T."
  (seq-drop-while (complement predicate) seq :key key))

(defun seq-split (n seq)
  "Split the sequence at the N-th element and return the both parts as a list."
  (do ((enum (seq-enum seq) (enum-cdr enum))
       (list nil (push (enum-car enum) list))
       (acc n (1- acc)))
      ((cond
        ((null enum) (return (list (nreverse list) nil)))
        ((zerop acc) (return (list (nreverse list) (make-seq enum))))
        (t nil)))))

(defun seq-split-if-not (predicate seq &key (key #'identity))
  "SEQ-SPLIT-IF-NOT takes a predicate function taking a single argument 
and a sequence. It splits the sequence at the first item for which the predicate
function returns NIL and then SEQ-SPLIT-IF-NOT returns the both parts as a list."
  (do ((enum (seq-enum seq) (enum-cdr enum))
       (list nil (push (enum-car enum) list)))
      ((cond
        ((null enum) 
         (return (list (nreverse list) nil)))
        ((not (funcall predicate (funcall key (enum-car enum)))) 
         (return (list (nreverse list) (make-seq enum))))
        (t nil)))))

(defun seq-split-if (predicate seq &key (key #'identity))
  "SEQ-SPLIT-IF takes a predicate function taking a single argument 
and a sequence. It splits the sequence at the first item for which the predicate
function returns T and and then SEQ-SPLIT-IF returns the both parts as a list."
  (seq-split-if-not (complement predicate) seq :key key))

(defun seq-null (seq)
  "Test whether the sequence is empty."
  (let ((enum (seq-enum seq)))
    (null enum)))

(defun seq-car (seq)
  "Return the head of sequence."
  (let ((enum (seq-enum seq)))
    (if (null enum) nil (enum-car enum))))

(defun seq-cdr (seq)
  "Return the tail of sequence."
  (let ((enum (seq-enum seq)))
    (if (null enum) nil (make-seq (enum-cdr enum)))))

(defun seq-repeatedly (function)
  "Return an infinite lazy sequence obtained by calling the function repeatedly."
  (make-seq
    (labels ((traverse ()
               (enum-cons 
                (funcall function)
                (traverse))))
      (traverse))))

(defun seq-iterate (function initial-value)
  "It returns an infinite lazy sequence obtained by starting with the supplied value, 
and then by calling the supplied function passing the previous item in the sequence as 
its argument."
  (make-seq
    (labels ((traverse (acc)
               (enum-cons
                acc
                (traverse (funcall function acc)))))
      (traverse initial-value))))

(defun seq-repeat (value)
  "It returns an infinite lazy sequence consisting of the argument value repeated endlessly."
  (make-seq
    (labels ((traverse ()
               (enum-cons
                value
                (traverse))))
      (traverse))))

(defun seq-range (&key (start 0) (end nil end-p) (step 1))
  "SEQ-RANGE returns a lazy sequence of numbers from the start (inclusive, 0 by default) 
to the end (exclusive, nil by default) incremented by the step (1 by default)."
  (make-seq
    (labels ((traverse (value)
               (cond
                ((and end-p (> step 0) (>= value end))
                 nil)
                ((and end-p (< step 0) (<= value end))
                 nil)
                (t (enum-cons
                    value
                    (traverse (+ value step)))))))
      (traverse start))))

(defun seq-cycle (seq)
  "It returns a lazy infinite sequence obtained by successively repeating 
the values in the supplied sequence."
  (make-seq
    (labels ((traverse (enum)
               (if (null enum)
                   (let ((enum (seq-enum seq)))
                     (if (null enum)
                         nil
                       (traverse enum)))
                 (enum-cons
                  (enum-car enum)
                  (traverse (enum-cdr enum))))))
      (traverse nil))))

(defun seq-interpose (value seq)
  "SEQ-INTERPOSE takes two arguments, a value and a sequence. It returns 
a lazy sequence obtained by inserting the supplied value between the values 
in the sequence."
  (make-seq
    (labels ((traverse (enum)
               (if (null enum)
                   nil
                 (let ((enum-car (enum-car enum))
                       (enum-cdr (enum-cdr enum)))
                   (if (null enum-cdr)
                       (enum-cons enum-car nil)
                     (enum-cons
                      enum-car
                      (enum-cons
                       value
                       (traverse enum-cdr))))))))
      (traverse (seq-enum seq)))))

(defun seq-member (item seq &key (test #'eql) (key #'identity))
  "Search a sequence for an item and return the tail of the sequence beginning with this element; 
otherwise NIL is returned."
  (do ((enum (seq-enum seq) (enum-cdr enum)))
      ((cond
        ((null enum) 
         (return nil))
        ((funcall test item (funcall key (enum-car enum)))
         (return (make-seq enum)))
        (t nil)))))

(defun seq-member-if (predicate seq &key (key #'identity))
  "Search a sequence for a top-level item for which the predicate returns T and
return the tail of the sequence beginning with this element; otherwise NIL is returned."
  (do ((enum (seq-enum seq) (enum-cdr enum)))
      ((cond
        ((null enum) 
         (return nil))
        ((funcall predicate (funcall key (enum-car enum)))
         (return (make-seq enum)))
        (t nil)))))

(defun seq-member-if-not (predicate seq &key (key #'identity))
  "Search a sequence for a top-level item for which the predicate returns NIL and
return the tail of the sequence beginning with this element; otherwise NIL is returned."
  (seq-member-if (complement predicate) seq :key key))

(defun enum-reduce (function enum &key key initial-value)
  "Like REDUCE but applied to the enumerators."
  (do ((enum enum (enum-cdr enum))
       (acc initial-value 
            (funcall function acc 
                     (funcall key (enum-car enum)))))
      ((null enum) (return acc))))

(defun seq-reduce (function seq &key (key #'identity) (initial-value nil initial-value-p))
  "Like REDUCE but applied to the generic sequences."
  (let ((enum (seq-enum seq)))
    (cond
     (initial-value-p
      (enum-reduce function enum
                   :key key
                   :initial-value initial-value))
     ((null enum) 
      nil)
     (t
      (enum-reduce function (enum-cdr enum)
                   :key key
                   :initial-value (funcall key (enum-car enum)))))))

(defun seq-position (item seq &key (test #'eql) (key #'identity))
  "Search a sequence for an element and return the index within the sequence; 
otherwise, NIL is returned."
  (do ((enum (seq-enum seq) (enum-cdr enum))
       (index 0 (1+ index)))
      ((cond
        ((null enum) 
         (return nil))
        ((funcall test item (funcall key (enum-car enum)))
         (return index))
        (t nil)))))

(defun seq-position-if (predicate seq &key (key #'identity))
  "Search a sequence for an element for which the predicate returns T and
return the index within the sequence; otherwise, NIL is returned."
  (do ((enum (seq-enum seq) (enum-cdr enum))
       (index 0 (1+ index)))
      ((cond
        ((null enum) 
         (return nil))
        ((funcall predicate (funcall key (enum-car enum)))
         (return index))
        (t nil)))))

(defun seq-position-if-not (predicate seq &key (key #'identity))
  "Search a sequence for an element for which the predicate returns NIL and
return the index within the sequence; otherwise, NIL is returned."
  (seq-position-if (complement predicate) seq :key key))

(defun seq-find (item seq &key (test #'eql) (key #'identity))
  "Search a sequence for an item and return this element; otherwise NIL is returned."
  (do ((enum (seq-enum seq) (enum-cdr enum)))
      ((cond
        ((null enum) 
         (return nil))
        ((funcall test item (funcall key (enum-car enum)))
         (return (enum-car enum)))
        (t nil)))))

(defun seq-find-if (predicate seq &key (key #'identity))
  "Search a sequence for an item for which the predicate returns T and 
return this element; otherwise NIL is returned."
  (do ((enum (seq-enum seq) (enum-cdr enum)))
      ((cond
        ((null enum) 
         (return nil))
        ((funcall predicate (funcall key (enum-car enum)))
         (return (enum-car enum)))
        (t nil)))))

(defun seq-find-if-not (predicate seq &key (key #'identity))
  "Search a sequence for an item for which the predicate returns NIL and 
return this element; otherwise NIL is returned."
  (seq-find-if (complement predicate) seq :key key))

(defun seq-every-1 (predicate seq)
  "Like EVERY but applied to one generic sequence."
  (do ((enum (seq-enum seq) (enum-cdr enum)))
      ((if (null enum)
           (return t)
         (let ((x (funcall predicate (enum-car enum))))
           (if x
               nil
             (return nil)))))))

(defun seq-some-1 (predicate seq)
  "Like SOME but applied to one generic sequence."
  (do ((enum (seq-enum seq) (enum-cdr enum)))
      ((if (null enum)
           (return nil)
         (let ((x (funcall predicate (enum-car enum))))
           (if x
               (return x)
             nil))))))

(defun seq-notany-1 (predicate seq)
  "Like NOTANY but applied to one generic sequence."
  (do ((enum (seq-enum seq) (enum-cdr enum)))
      ((if (null enum)
           (return t)
         (let ((x (funcall predicate (enum-car enum))))
           (if x
               (return nil)
             nil))))))

(defun seq-notevery-1 (predicate seq)
  "Like NOTEVERY but applied to one generic sequence."
  (do ((enum (seq-enum seq) (enum-cdr enum)))
      ((if (null enum)
           (return nil)
         (let ((x (funcall predicate (enum-car enum))))
           (if x
               nil
             (return t)))))))

(defun seq-every (predicate &rest seqs)
  "Like the EVERY function but applied to generic sequences."
  (assert (not (null seqs)))
  (if (null (cdr seqs))
      (seq-every-1 predicate (car seqs))
    (seq-every-1 (lambda (xs) (apply predicate xs))
                 (apply #'seq-zip seqs))))

(defun seq-some (predicate &rest seqs)
  "Like the SOME function but applied to generic sequences."
  (assert (not (null seqs)))
  (if (null (cdr seqs))
      (seq-some-1 predicate (car seqs))
    (seq-some-1 (lambda (xs) (apply predicate xs))
                (apply #'seq-zip seqs))))

(defun seq-notany (predicate &rest seqs)
  "Like the NOTANY function but applied to generic sequences."
  (assert (not (null seqs)))
  (if (null (cdr seqs))
      (seq-notany-1 predicate (car seqs))
    (seq-notany-1 (lambda (xs) (apply predicate xs))
                  (apply #'seq-zip seqs))))

(defun seq-notevery (predicate &rest seqs)
  "Like the NOTEVERY function but applied to generic sequences."
  (assert (not (null seqs)))
  (if (null (cdr seqs))
      (seq-notevery-1 predicate (car seqs))
    (seq-notevery-1 (lambda (xs) (apply predicate xs))
                    (apply #'seq-zip seqs))))

;;;
;;; List
;;;

(defmethod seqp ((seq list))
  t)

(defmethod seq-enum ((seq list))
  (labels ((traverse (list)
             (if (null list)
                 nil
               (enum-cons
                (car list)
                (traverse (cdr list))))))
    (traverse seq)))

;;;
;;; Vector
;;;

(defmethod seqp ((seq vector))
  t)

(defmethod seq-enum ((seq vector))
  (let ((len (length seq)))
    (labels ((traverse (index)
               (if (= index len)
                   nil
                 (enum-cons
                  (aref seq index)
                  (traverse (1+ index))))))
      (traverse 0))))
