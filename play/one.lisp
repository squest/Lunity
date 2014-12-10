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

(defmacro ->> (&body body)
  (labels ((looper (ls res)
	      (if (null ls)
		  res
		  (looper (rest ls)
		     (append (first ls) (list res))))))
    (looper (rest body) (first body))))

(defmacro -> (&body body)
  (labels ((looper (ls res)
	      (if (null ls)
		  res
		  (looper (rest ls)
		     (append (list (first (first ls)))
			     (list res)
			     (rest (first ls)))))))
    (looper (rest body) (first body))))

(defun take-odd (xs)
  (deff)
  (labels ((looper (ls res)
	      (deff)
	      (if (null ls)
		  res
		  (looper (rest (rest ls))
		     (cons (first ls) res)))))
    (looper (rest (reverse xs)) nil)))

(defun take-even (xs)
  (deff)
  (labels ((looper (ls res)
	      (deff)
	      (if (or (= 1 (length ls)) (null ls))
		  res
		  (looper (rest (rest ls))
		     (cons (first ls) res)))))
    (looper (reverse xs) nil)))

(defmacro cloop (lbinding ldef lbody)
  `(labels ((recur ,(take-odd lbinding) ,ldef
		   ,lbody))
     ,(cons 'recur (take-even lbinding))))

(defmacro clet (lbinding &body lbody)
  `(let* ,(cmap (fn2 (list %1 %2))
		(take-odd lbinding)
		(take-even lbinding))
     ,@lbody))

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

(defgeneric nil? (lseq))
(defgeneric hd (lseq))
(defgeneric tl (lseq))
(defgeneric rev (lseq))
(defgeneric clast (lseq))
(defgeneric force (lseq))


(defmethod hd ((lst lazy-seq))
  (deff)
  (first (slot-value lst 'val)))

(defmethod hd ((lst lazy-vec))
  (deff)
  (clet (tmp (funcall (first (slot-value lst 'val))))
    (if (typep tmp 'lazy-vec)
	(hd tmp)
	(first tmp))))

(defmethod force ((lst lazy-vec))
  (deff)
  (clet (tmp (funcall (first (slot-value lst 'val))))
    (if (typep tmp 'lazy-vec)
	(append (force tmp)
		(list (second (slot-value lst 'val))))
	(if (listp tmp)
	    tmp
	    (list tmp)))))

(defmethod tl ((lst lazy-seq))
  (deff)
  (funcall (rest (slot-value lst 'val))))

(defmethod force ((lst lazy-seq))
  (deff)
  (cons (hd lst) (tl lst)))

(defmacro lcons (thead ttail)
  `(make-instance 'lazy-seq
		  :val (cons ,thead (lambda () ,ttail))))

(defmethod rev ((lst lazy-seq))
  (deff)
  (reverse (cons (hd lst) (tl lst))))

(defmethod rev ((lst lazy-vec))
  (deff)
  (reverse (force lst)))


(defmethod clast ((lst lazy-seq))
  (deff)
  (first (last (tl lst))))

(defmethod clast ((lst lazy-vec))
  (deff)
  (first (last (slot-value lst 'val))))

(defun iterate (f i)
  (deff)
  (lcons i (iterate f (funcall f i))))

(defun lrange (&optional start step)
  (deff start step)
  (if step
      (lcons start (lrange (+ start step) step))
      (if start
	  (lcons start (lrange (inc start) 1))
	  (lcons 0 (lrange 1 1)))))

(defun range (&rest args)
  (deff)
  (cond ((= 1 (length args))
	 (loop for i from 0 to (first args) collect i))
	((= 2 (length args))
	 (loop for i from (first args) to (second args) collect i))
	((= 3 (length args))
	 (let ((a (first args))
	       (b (second args)))
	   (if (<= a b)
	       (loop for i from a to b by (third args) collect i)
	       (loop for i from a downto b by (third args) collect i))))))

(defun take (n lst)
  (deff n)
  (if (= n 0)
      '()
      (cons (hd lst) (take (dec n) (tl lst)))))












