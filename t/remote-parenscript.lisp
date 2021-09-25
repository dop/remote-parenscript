(in-package #:remote-parenscript/tests)

(in-suite remote-parenscript)

(defparameter *rps* nil)

(defmacro rps (&body body)
  "Convenience wrapper to compile and execute Parenscript code in *RPS* evaluator."
  `(rps:ps (:env *rps*) ,@body))

(defmacro with-rps ((&optional rps) &body body)
  `(let ((*rps* ,(or rps `*rps*)))
     (unwind-protect
          (progn ,@body)
       (rps:stop *rps*))))

(def-test chromium-construct-array ()
  (with-rps ((rps:start :runner (make-instance 'r:chromium :headless t) :wait t))
    (is (equal '(1 2 3 4 5)
               (rps (labels ((fill (arr x)
                               (funcall (ps:@ arr fill) x))
                             (map (arr fn)
                               (funcall (ps:@ arr map) fn)))
                      (let ((arr (ps:new (-array 5))))
                        (map (fill arr 0) (lambda (x i) (1+ i))))))))))

(def-test node-construct-array ()
  (with-rps ((rps:start :runner (make-instance 'r:node) :wait t))
    (is (equal '(1 2 3 4 5)
               (rps (labels ((fill (arr x)
                               (funcall (ps:@ arr fill) x))
                             (map (arr fn)
                               (funcall (ps:@ arr map) fn)))
                      (let ((arr (ps:new (-array 5))))
                        (map (fill arr 0) (lambda (x i) (1+ i))))))))))


