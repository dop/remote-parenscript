(in-package #:remote-parenscript-utils)

(defmacro destructuring-lambda (bindings &body body)
  `(lambda (&rest args)
     (destructuring-bind ,bindings args
       ,@body)))

(defpsmacro with-promise (p k fn &rest args)
  (reduce (destructuring-lambda (acc (k fn))
            `(funcall (getprop ,acc ,k) ,fn))
          (loop for (a b) on args by #'cddr collect (list a b))
          :initial-value `(funcall (getprop ,p ,k) ,fn)))

(defpsmacro send-back (expr)
  `(funcall (@ -remote-j-s send) ,expr))

(defpsmacro console-log (&rest exprs)
  `(funcall (@ console log) ,@exprs))

(defpsmacro to-string (expr)
  `(funcall (@ ,expr to-string)))

(defpsmacro json-stringify (expr)
  `(funcall (@ *JSON* stringify) ,expr))
