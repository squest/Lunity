(defclass lcons ()
  ((val :initarg :val)))


(defconstant true t)
(defconstant false nil)

(defun clojure-vector-char (stream char)
  `',(read-delimited-list #\] stream t))

(set-macro-character #\[ #'clojure-vector-char)
(set-macro-character #\] (get-macro-character #\)))

(defmacro deff (&rest ftypes)
  (if (null ftypes)
      `(list `declare `(optimize (speed 3)))
      `(list `declare `(optimize (speed 3))
	     (cons `fixnum (list ,@ftypes)))))

(defun squares (lim)
  (deff lim)
  (mapcar #'(lambda (x) (* x x)) '(1 2 4 5)))

(defmacro def (vname vbody)
  `(defparameter ,vname ,vbody))

(defmacro fn (body)
  `(lambda (%) ,body))

(defmacro fn2 (body)
  `(lambda (%1 %2) ,body))

(defmacro fn3 (body)
  `(lambda (%1 %2 %3) ,body))

(defmacro cmap (f &rest lst)
  `(mapcar ,f ,@lst))

(defun inc (x) (1+ x))
(defun dec (x) (1- x))

(defun div (a m)
  (truncate (/ a m)))

(defclass lazy-seq ()
  ((val :initarg :val)))

(defclass lazy-vec ()
  ((val :initarg :val)))

(defmacro lseq (thead &rest ttail)
  `(make-instance 'lazy-seq
		  :val (cons ,thead (lambda () (list ,@ttail)))))

(defmacro lvec (thead &rest ttail)
  (let ((tmp (cons thead ttail)))
    `(make-instance 'lazy-vec
		    :val (append (lambda () (butlast ,tmp))
				 (last ,tmp)))))

(defmacro conj (lst a)
  `(make-instance 'lazy-vec
		  :val (append (lambda () ,lst)
			       (list ,a))))

(defgeneric nil? (lseq))

(defgeneric hd (lseq))
(defgeneric tl (lseq))
(defgeneric rev (lseq))
(defgeneric clast (lseq))
(defgeneric force (lseq))

(defmethod hd ((lst lazy-seq))
  (deff)
  (first (slot-value lst 'val)))

(defmethod tl ((lst lazy-seq))
  (deff)
  (funcall (rest (slot-value lst 'val))))

(defmethod force ((lst lazy-seq))
  (deff)
  (cons (hd lst) (tl lst)))

(defmacro lcons (thead ttail)
  `(make-instance 'lazy-seq
		  :val (cons ,thead (lambda () (force ,ttail)))))

(defmethod rev ((lst lazy-seq))
  (deff)
  (reverse (cons (hd lst) (tl lst))))

(defmethod clast ((lst lazy-seq))
  (deff)
  (first (last (tl lst))))

(defmethod clast ((lst lazy-vec))
  (deff)
  (first (last (slot-value lst 'val))))








