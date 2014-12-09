;; This is an attempt to make some of clojure functions runs on SBCL

;; Basic rules: if the name of a macro/function does not exist in
;; standard CL then it will be used. If such symbol already used in CL
;; vocab, then we'll use a prefix c... for example:

;; range doesnt exist in CL therefore we use range however let already
;; exists in CL therefore we use clet. if you're not sure, then you
;; can simply use c prefix for all clojure built-in macros and
;; functions, and you're good to go.


(defconstant true t)
(defconstant false nil)

(defun clojure-vector-char (stream char)
  `',(read-delimited-list #\] stream t))

(set-macro-character #\[ #'clojure-vector-char)
(set-macro-character #\] (get-macro-character #\)))

(defmacro defn (fname fbind &rest fbody)
  `(defun ,fname ,(list @fbind)
     ,fbody))

(defmacro deff (&rest ftypes)
  (declare (optimize (speed 3))
	   `(fixnum ,@ftypes)))

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

(defun hd (lst)
  (deff)
  (first lst))

(defun rev (lst)
  (deff)
  (reverse lst))

(defun conj (xs &rest x)
  (deff)
  (append xs x))

(defun clast (lst)
  (deff)
  (car (last lst)))

(defun take (n xs)
  (deff n)
  (labels ((looper (i lxs res)
	      (deff i)
	      (if (or (null lxs) (> i n))
		  res
		  (looper (inc i)
		     (rest lxs)
		     (append res (list (first lxs)))))))
    (looper 1 xs nil)))

(defun ctake (n xs)
  (deff n)
  (labels ((looper (i lxs res)
	      (deff i)
	      (if (or (null lxs) (> i n))
		  res
		  (looper (inc i)
		     (rest lxs)
		     (append res (list (first lxs)))))))
    (looper 1 xs nil)))

(defun drop (n xs)
  (deff n)
  (if (= 0 n)
      xs
      (drop (dec n) (rest xs))))

(defun cdrop (n xs)
  (deff n)
  (if (= 0 n)
      xs
      (cdrop (dec n) (rest xs))))

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

(defun take-while (f lst)
  (cloop (lxs lst res nil) (deff)
    (cond ((null lxs) res)
	  ((not (funcall f (first lxs))) res)
	  (:else (recur (rest lxs)
			(append res (list (first lxs))))))))

(defun ctake-while (f lst)
  (cloop (lxs lst res nil) (deff)
    (cond ((null lxs) res)
	  ((not (funcall f (first lxs))) res)
	  (:else (recur (rest lxs)
			(append res (list (first lxs))))))))

(defmacro clet (lbinding &body lbody)
  `(let* ,(cmap (fn2 (list %1 %2))
		  (take-odd lbinding)
		  (take-even lbinding))
     ,@lbody))

(defun drop-while (f lst)
  (if (null lst)
      '()
      (if (funcall f (first lst))
	  (drop-while f (rest lst))
	  lst)))

(defun cdrop-while (f lst)
  (if (null lst)
      '()
      (if (funcall f (first lst))
	  (cdrop-while f (rest lst))
	  lst)))

(defun zero? (x) (zerop x))

(defun true? (x) (equal true x))

(defun false? (x) (not x))

(defun nil? (x) (null x))

(defun empty? (x) (null x))

(defun quot (a m)
  "Integer division, div"
  (deff a m)
  (floor (/ a m)))

(defun filter (f ls)
  "Remove-if-not f ls"
  (deff)
  (remove-if-not f ls))

(defun sum (ls)
  "Returns the sum of all elements in ls"
  (deff)
  (apply '+ ls))

(defun product (ls)
  "Returns the product of all elements in ls"
  (deff)
  (apply '* ls))

(defun div? (a m)
  "Returns true if a is evenly-divisible by m"
  (deff a m)
  (zerop (rem a m)))


(defun permute (ls)
  "Returns all possible permutations of ls"
  (deff)
  (if (= 1 (length ls))
      (mapcar 'list ls)
      (loop for i in ls
	 append (loop for rs in (permute (remove i ls))
		   collect (cons i rs)))))

(defun combine (ls n)
  "Takes n combinations of ls"
  (deff n)
  (if (= 0 n)
      '(())
      (loop for i in ls
	    for j from 1 to (length ls)
	 append (loop for rs in (combine (drop j ls)
					 (dec n))
		   collect (cons i rs)))))

(defun iterate (fn i gn)
  "Returns non-lazy iterate while (gn i) is true"
  (if (not (funcall gn i))
      nil
      (cons i (iterate fn (funcall fn i) gn))))

(defun every? (fn ls)
  "Returns true if every element in ls satisfies fn"
  (deff)
  (if (empty? ls)
      true
      (if (not (funcall fn (first ls)))
	  false
	  (every? fn (rest ls)))))

(defun some? (fn ls)
  "Returns true if at least one  element in ls satisfies fn"
  (deff)
  (if (empty? ls)
      false
      (if (funcall fn (first ls))
	  true
	  (every? fn (rest ls)))))

(defun partial (fn &rest args)
  "Returns a curried version of fn"
  (deff)
  (lambda (&rest xs) (apply fn (append args xs))))

(defun part (fn &rest args)
  "Returns a curried version of fn"
  (deff)
  (lambda (&rest xs) (apply fn (append args xs))))

(defun comp-helper (ls)
  (deff)
  (if (= 1 (length ls))
      (lambda (x) (funcall (first ls) x))
      (lambda (x) (funcall (comp-helper (rest ls))
		      (funcall (first ls) x)))))

(defun comp (&rest args)
  "Clojure's comp with standard clisp behaviour (you need to call it with funcall)"
  (deff)
  (comp-helper (reverse args)))

(defun juxt-helper (ls x)
  (deff)
  (if (empty? ls)
      nil
      (cons (funcall (first ls) x)
	    (juxt-helper (rest ls) x))))

(defun juxt (&rest ls)
  "Clojure's juxt with clisp behaviour"
  (deff)
  (lambda (x) (juxt-helper ls x)))

(defun spit (fname obj)
  "Clojure spit to file behaviour"
  (with-open-file (outfile fname
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
    (prin1 obj outfile)))

(defun take-lim (n ls)
  "Returns the elements in ls that less than n"
  (deff)
  (labels ((helper (ls res)
	     (if (> (first ls) n)
		 res
		 (helper (rest ls) (append res (list (first ls)))))))
    (helper ls nil)))









