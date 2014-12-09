(load "clojure.lisp")

(defun mone (a m)
  (clet (b (* a a)
	   n (* m m))
    (list a m b n)))

(defun squares (n lim)
  (deff a lim)
  (cloop (i 1 res nil) (deff i res)
      (if (> i lim)
	  res
	  (clet (tmp (cloop (m 1 res 1) (deff m res)
		       (if (> m n)
			   res
			   (recur (inc m)
				  (* res i)))))
	    (recur (inc i) (conj res tmp))))))











