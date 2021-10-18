(in-package #:remote-parenscript/tests)

(in-suite remote-parenscript)

(defparameter *rps* nil)

(defmacro rps (&body body)
  "Convenience wrapper to compile and execute Parenscript code in *RPS* evaluator."
  `(rps:ps (:env *rps*) ,@body))

(def-test chromium-construct-array ()
  (with-chromium ()
    (is (equal '(1 2 3 4 5)
               (rps (labels ((fill (arr x)
                               (funcall (ps:@ arr fill) x))
                             (map (arr fn)
                               (funcall (ps:@ arr map) fn)))
                      (let ((arr (ps:new (-array 5))))
                        (map (fill arr 0) (lambda (x i) (1+ i))))))))))

(def-test chromium-global-function ()
  (with-chromium ()
    (rps (defun get-random-number () 4))
    (is (equal 4 (rps (funcall (ps:@ window get-random-number)))))))

(def-test node-construct-array ()
  (with-node ()
    (is (equal '(1 2 3 4 5)
               (rps (labels ((fill (arr x)
                               (funcall (ps:@ arr fill) x))
                             (map (arr fn)
                               (funcall (ps:@ arr map) fn)))
                      (let ((arr (ps:new (-array 5))))
                        (map (fill arr 0) (lambda (x i) (1+ i))))))))))

(def-test node-global-function ()
  (with-node ()
    (rps (defun get-random-number () 4))
    (is (equal 4 (rps (funcall (ps:@ global get-random-number)))))))

