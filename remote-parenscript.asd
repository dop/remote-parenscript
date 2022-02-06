(in-package #:asdf-user)

(defsystem :remote-parenscript
  :description ""
  :depends-on (#:remote-js
               #:find-port
               #:parenscript
               #:log4cl
               #:cl-json
               #:lparallel
               #:alexandria
               #:anaphora)
  :pathname "src"
  :serial t
  :components ((:file "ps-packages")
               (:file "ps-utils")
               (:file "runners")
               (:file "remote-parenscript"))
  :in-order-to ((test-op (test-op :remote-parenscript/tests))))

(defsystem :remote-parenscript/tests
  :depends-on (#:remote-parenscript #:fiveam #:trivia #:exit-hooks)
  :pathname "t"
  :serial t
  :components ((:file "setup")
               (:file "runners")
               (:file "remote-parenscript-utils")
	       (:file "remote-parenscript"))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run-all-tests)))
