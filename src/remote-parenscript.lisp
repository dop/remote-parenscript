(defpackage #:remote-parenscript
  (:use #:cl)
  (:nicknames #:rps)
  (:local-nicknames (#:js #:remote-parenscript-utils)
                    (#:r #:remote-parenscript-runners))
  (:export #:start #:stop #:ps #:ps*)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:anaphora #:it))

(in-package #:remote-parenscript)

(defparameter *last-evaluator* nil
  "Convenience parameter to save last evaluator object so that ENV parameter can be skipped when calling STOP, PS* functions or PS macro.")

(defclass remote-parenscript-evaluator ()
  ((remote-js-ctx
    :initarg :ctx
    :reader rps-ctx)
   (result-id
    :initform 0
    :accessor rps-result-id)
   (results
    :initarg :results
    :type hash-table
    :accessor rps-results)
   (runner
    :initarg :runner
    :initform nil
    :reader rps-runner)))

(defmacro with-it (expr &body body)
  `(let* ((it ,expr)
          ,@(loop for binding in body
                  collect (list 'it binding)))
     it))

(defmacro with-cond-it (expr &body body)
  `(let* ((it ,expr)
          ,@(loop for (condition binding) in body
                  collect `(it (if ,condition ,binding it))))
     it))

(defun ensure-runnable-code (code)
  "Ensure that CODE is runnable Parenscript.

Atoms are wrapped in PROGN and strings in EVAL."
  (cond ((stringp code)
	 `(funcall (ps:@ window eval) ,code))
	((atom code)
	 (list 'progn code))
	(t
	 code)))

(defun wrap-in-error-handling (code &key log-error)
  `(ps:try ,(ensure-runnable-code code)
	   (:catch (e)
	     ,(if log-error
		`(let ((error-string (js::to-string e)))
		   (js::send-back error-string)
		   (js::console-log error-string)
		   error-string)
		`(js::to-string e)))))

(defun wrap-in-send-back (id code)
  (with-gensyms (result data send)
    `(let ((,result ,code)
           (,send (lambda (,data)
                    (js::send-back (js::json-stringify (list ,id ,data))))))
       (if (ps:instanceof ,result -promise)
           (js::with-promise ,result 'then ,send 'catch ,send)
           (funcall ,send ,result)))))

(defun wrap-code (code &key send-back handle-error)
  (with-cond-it code
    (handle-error (wrap-in-error-handling it :log-error (not send-back)))
    (send-back (wrap-in-send-back send-back it))))

(defun ps* (code &key (env *last-evaluator*) (handle-error t) (send-back t) (wait t) (timeout 3.0))
  "Evaluate CODE in ENV evaluator.

With HANDLE-ERROR (defaults to T) wraps CODE in try/catch.
With SEND-BACK (defaults to T) sends back result of CODE evaluation.
WAIT will block execution until result is received or TIMEOUT reached."
  (declare (type remote-parenscript-evaluator env))
  (let ((id (incf (rps-result-id env))))
    (symbol-macrolet ((future-result (gethash id (rps-results env))))
      (setf future-result (lparallel:promise))
      (with-it (wrap-code code :send-back (and send-back id)
                               :handle-error handle-error)
        (remote-js:eval (rps-ctx env) (ps:ps* it))
        (log:debug it))
      (cond ((and send-back wait)
             (bt:make-thread
              #'(lambda ()
                  (sleep timeout)
                  (unless (lparallel:fulfilledp future-result)
                    (lparallel:fulfill future-result :timeout))))
             (lparallel:force future-result))
	    (wait
	     (log:warn "Cannot wait because" send-back))
	    (t
	     future-result)))))

(defun make-defun-global (expr)
  "If EXPR is PS:DEFUN or CL:DEFUN put it in PROGN and prepend SETF
expresion for setting window variable with same symbol name as defun."
  (if (and (consp expr) (member (car expr) '(cl:defun ps:defun)))
      `(progn
         (setf (ps:@ window ,(second expr)) ,(second expr))
         ,expr)
      expr))

(defmacro ps ((&key (env *last-evaluator*) (handle-error t) (send-back t) (wait t) (timeout 3.0)) &body body)
  "Compiles BODY using Parenscript if it is not a string and sends to
runner for evaluation using JavaScript evaluator ENV."
  `(ps* ',(let ((code (mapcar #'make-defun-global body)))
            (if (= 1 (length code))
                (first code)
                (cons 'progn code)))
        :env ,env
        :handle-error ,handle-error
        :send-back ,send-back
        :wait ,wait
        :timeout ,timeout))

(defun browse-temporary-html (html &key headless)
  "Generate temporary HTML file and open it with chromium. Pass
--headless flag if HEADLESS is not nil."
  (uiop:with-temporary-file (:pathname filepath :stream out :type "html" :keep t)
    (write-string html out)
    (sb-ext:run-program "chromium" (append (when headless (list "--headless" "--remote-debugging-port=9222"))
                                           (list (namestring filepath)))
                        :search t
                        :wait nil)))

(defvar *results-table* nil
  "For binding result hash table in MESSAGE-HANDLER.")

(defun message-handler (js-message)
  "A callback for Remote-JS context that receives a message JS-MESSAGE back from runner.

If message is an array of two elements with first one being a number,
then second item is used to fullfil promise stored in *RESULTS-TABLE*
at key provided that number.

Etherwise JS-MESSAGE is simply logged."
  (declare (type string js-message))
  (handler-case
      (let ((json (cl-json:decode-json-from-string js-message)))
	(if (and (consp json)
		 (= 2 (length json))
		 (typep (car json) 'number))
            (let* ((id (first json))
		   (value (second json))
                   (promise (gethash id *results-table*)))
              (log:debug id value)
              (if (lparallel:fulfilledp promise)
                  (log:debug "Promise already fulfilled" promise)
                  (lparallel:fulfill promise value))
              (remhash id *results-table*))
            (log:info json)))
    (error ()
      (log:info js-message))))

(defun start (&key (runner (make-instance 'r:chromium)) (port (find-port:find-port)) wait)
  "Start Remote-JS server and optionally start evaluator process.

Returns REMOTE-PARENSCRIPT-EVALUATOR object.

If RUNNER is given, starts it to connect to Remote-JS server."
  (let* ((results (make-hash-table))
         (ctx (remote-js:make-buffered-context
               :port port
               :timeout 10000
               :callback (lambda (message)
                           (let ((*results-table* results))
                             (message-handler message))))))
    (remote-js:start ctx)
    (when runner
      (r:start runner ctx)
      (when wait
        (loop until (remote-js:context-connected-p ctx)
              do (sleep 0.2))))
    (setf *last-evaluator*
          (make-instance 'remote-parenscript-evaluator
                         :ctx ctx
                         :results results
                         :runner runner))))

(defun stop (&optional (env *last-evaluator*))
  "Stop Remote-JS server and runner process if it was started."
  (declare (type remote-parenscript-evaluator env))
  (remote-js:stop (rps-ctx env))
  (when (rps-runner env)
    (r:stop (rps-runner env))))
