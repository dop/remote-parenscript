(in-package #:remote-parenscript/tests)

(in-suite remote-parenscript)

(defparameter *headless* t)

;; For development it's convenient to start browser for JavaScript
;; evaluation and set *JS* to JS-EVALUATOR object. Then just execute
;; tests one at a time and it will reuse opened browser.
(defparameter *rps* nil)

(defmacro rps (&body body)
  "Convenience wrapper to compile and execute Parenscript code in *RPS* evaluator."
  `(rps:ps (:env *rps*) ,@body))

(defun boot ()
  "Create, start, and wait for REMOTE-PARENSCRIPT-EVALUATOR to be ready."
  (let ((env (rps:start))
        ;;(code (ps:ps-compile-file (merge-pathnames "src/ps-utils.lisp" (asdf:system-source-directory "remote-parenscript"))))
        )
    (loop until (rps:connected-p env)
          do (sleep 1) ;; finally (rps:ps* code :env env)
          )
    env))

(defmacro with-fresh-rps (() &body body)
  `(let ((*rps* (boot)))
     (unwind-protect
          (progn ,@body)
       (rps:stop *rps*))))

(defmacro with-rps (() &body body)
  `(if *rps*
       (progn ,@body)
       (with-fresh-rps () ,@body)))

(defmacro def-ps-test (name args &body body)
  `(def-test ,name ,args
     (with-rps ()
       ,@body)))

(def-ps-test construct-array ()
  (is (equal '(1 2 3 4 5)
             (rps (labels ((fill (arr x)
                             (funcall (ps:@ arr fill) x))
                           (map (arr fn)
                             (funcall (ps:@ arr map) fn)))
                    (let ((arr (ps:new (-array 5))))
                      (map (fill arr 0) (lambda (x i) (1+ i)))))))))
