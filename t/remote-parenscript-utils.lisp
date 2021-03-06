(in-package #:remote-parenscript/tests)

(in-suite remote-parenscript)

(def-test ensure-runnable-code ()
  (is (ps= '(funcall (ps:@ window eval) "1") (rps::ensure-runnable-code "1")))
  (is (equal '(progn 1) (rps::ensure-runnable-code 1)))
  (is (equal '(alert "hello") (rps::ensure-runnable-code '(alert "hello")))))

(def-test wrap-in-error-handling ()
  (is (ps= '(try (alert 1) (:catch (e) (to-string e)))
           (rps::wrap-in-error-handling '(alert 1))))
  (is (ps= '(try (alert 1)
             (:catch (e)
               (let ((error-string (to-string e)))
                 (send-back error-string)
                 (console-log error-string)
                 error-string)))
           (rps::wrap-in-error-handling '(alert 1) :log-error t))))

(def-test wrap-in-send-back ()
  (is (ps= '(let ((_result (+ 1 2))
                  (_send (lambda (_data)
                           (send-back (json-stringify (list 123 _data))))))
             (if (instanceof _result -promise)
                 (with-promise _result 'then _send 'catch _send)
                 (funcall _send _result)))
           (rps::wrap-in-send-back 123 '(+ 1 2)))))

(def-test wrap-code ()
  (let ((code '(alert :key)))
    (is (equal (rps::wrap-code code) code))
    (is (ps= (rps::wrap-in-send-back 1 code)
             (rps::wrap-code code :send-back 1)))
    ;; Error handling without sending result back has to at least log
    ;; the error to console.
    (is (ps= (rps::wrap-in-error-handling code :log-error t)
             (rps::wrap-code code :handle-error t)))
    (is (ps= (rps::wrap-in-send-back 1 (rps::wrap-in-error-handling code))
             (rps::wrap-code code :send-back 1 :handle-error t)))))
