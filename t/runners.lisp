(in-package #:remote-parenscript/tests)

(def-test runner-global ()
  (is (eq 'ps-window-wd-symbols:window (r:global nil))))
