(defpackage #:remote-parenscript-runners
  (:use #:cl)
  (:import-from #:alexandria #:appendf)
  (:export #:runner #:start #:stop #:chromium #:node))

(in-package #:remote-parenscript-runners)

(defclass runner ()
  ((program
    :type string
    :initarg :program)
   (args
    :type list
    :initform nil
    :initarg :arguments)
   (process
    :accessor runner-process)))

(defgeneric start (o ctx &rest args))

(defgeneric stop (o))

(defmethod start ((o runner) (ctx remote-js:context) &rest more-args)
  (declare (ignore ctx))
  (with-slots (program args) o
    (setf (runner-process o)
          (sb-ext:run-program program (append args more-args) :search t :wait nil))))

(defmethod stop ((o runner))
  (let ((p (runner-process o)))
    (sb-ext:process-kill p 15 :process-group)
    (sb-ext:process-wait p)))

(defclass chromium (runner)
  ((headless
    :type boolean
    :initform nil
    :initarg :headless)
   (html-filepath
    :type string))
  (:default-initargs :program "chromium"))

(defmethod start ((o chromium) (ctx remote-js:context) &rest more-args)
  (uiop:with-temporary-file (:pathname filepath :stream out :type "html" :keep t)
    (write-string (remote-js:html ctx) out)
    (setf (slot-value o 'html-filepath) filepath)
    (apply #'call-next-method o ctx (append (when (slot-value o 'headless)
                                              (list "--headless" "--remote-debugging-port=9222"))
                                            more-args
                                            (list (namestring filepath))))))

(defmethod stop ((o chromium))
  (call-next-method)
  (uiop:delete-file-if-exists (slot-value o 'html-filepath)))

(defclass node (runner)
  ()
  (:default-initargs :program "node"))

(defmethod start ((o node) (ctx remote-js:context) &rest more-args)
  (let ((script (merge-pathnames "node-runner" (asdf:system-source-directory "remote-parenscript"))))
    (apply #'call-next-method o ctx (list* (namestring script)
                                           "--port" (format nil "~D" (remote-js:context-port ctx))
                                           more-args))))
