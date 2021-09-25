(in-package #:remote-parenscript-utils)

(defpsmacro defun! (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list ,@body)
     (setf (@ window ,name) ,name)))

(defpsmacro with-deferred-promise
    ((&key (resolve 'resolve) (reject 'reject)
           (promise (cl:gensym "promise"))
           (deferred-promise (cl:gensym "deferred-promise")))
     &body body)
  `(let* ((,deferred-promise (-deferred-promise))
          (,promise (@ ,deferred-promise promise)))
     (with-slots ((,resolve resolve) (,reject reject)) ,deferred-promise
       ,@body)
     ,promise))

(defpsmacro -> (x &rest forms)
  (if (cl:first forms)
      (let* ((form (cl:first forms))
             (threaded (if (cl:listp form)
                           (if (member '_ form)
                               `(funcall (lambda (_) ,form) ,x)
                               `(,(cl:first form) ,@(cl:rest form) ,x))
                           `(,form ,x))))
        `(-> ,threaded ,@(cl:rest forms)))
      x))

(defmacro destructuring-lambda (bindings &body body)
  `(lambda (&rest args)
     (destructuring-bind ,bindings args
       ,@body)))

(defpsmacro with-promise (p k fn &rest args)
  (reduce (destructuring-lambda (acc (k fn))
            `(funcall (getprop ,acc ,k) ,fn))
          (loop for (a b) on args by #'cddr collect (list a b))
          :initial-value `(funcall (getprop ,p ,k) ,fn)))

(defpsmacro array-index-of (array item)
  `(funcall (@ ,array index-of) ,item))

(defpsmacro array-push (array &rest args)
  `(funcall (@ ,array push) ,@args))

(defpsmacro array-map (fn array)
  `((@ ,array map) ,fn))

(defpsmacro array-splice (array from &optional limit &rest items)
  (cl:cond
    (items
     `(apply (@ ,array splice) ,from ,limit ,items))
    (limit
     `(funcall (@ ,array splice) ,from ,limit))
    (t
     `(funcall (@ ,array splice) ,from))))

(defpsmacro text-content (el)
  `(@ ,el text-content))

(defpsmacro get-by-id (id)
  `(funcall (@ document get-element-by-id) ,id))

(defpsmacro json-parse (str)
  `(funcall (@ *json* parse) ,str))

(defpsmacro append-child (el child)
  `(funcall (@ ,el append-child) ,child))

(defpsmacro ploop ((plist key value) &body body)
  (ps:with-ps-gensyms (i)
    `(loop with ,i = 0
           for ,key = (elt ,plist ,i) then (elt ,plist (incf ,i 2))
           while ,key do
             (let ((,value (elt ,plist (1+ ,i))))
               ,@body))))

(defpsmacro on (element event-name callback)
  `(funcall (@ ,element add-event-listener) ,event-name ,callback))

(defpsmacro send-back (expr)
  `(funcall (@ -remote-j-s send) ,expr))

(defpsmacro console-log (&rest exprs)
  `(funcall (@ console log) ,@exprs))

(defpsmacro log (&rest args)
  `(funcall (@ console log) ,@args))

(defpsmacro to-string (expr)
  `(funcall (@ ,expr to-string)))

(defpsmacro json-stringify (expr)
  `(funcall (@ *JSON* stringify) ,expr))
