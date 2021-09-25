(defpackage #:remote-parenscript/tests
  (:use #:cl #:fiveam #:trivia)
  (:local-nicknames (#:r #:remote-parenscript-runners)))

(in-package #:remote-parenscript/tests)

(def-suite remote-parenscript)

(defvar *ps-placeholder-map* nil
  "Alist of mapping between symbols that are supposed to be treated as equal.")

(defun gensym-symbol-p (symb)
  (declare (type symbol symb))
  (let* ((name (symbol-name symb))
         (start (position-if #'digit-char-p name))
         (end (and start (position-if-not #'digit-char-p name :start start))))
    (when (and start (plusp start) (not end))
      (values (subseq name 0 start) (subseq name start)))))

(defun ps-symbol-equal (template code)
  (match (assoc template *ps-placeholder-map*)
    ((cons _ (eq code))
     t)
    (nil
     (let ((template-name (symbol-name template)))
       (if (or (eql #\_ (elt template-name 0))
               (gensym-symbol-p template))
           (push (cons template code) *ps-placeholder-map*)
           (string= template-name (symbol-name code)))))))

#+5am
(let ((*ps-placeholder-map* '((a . b))))
  (assert (ps-symbol-equal 'a 'b))
  (assert (not (ps-symbol-equal 'b 'a))))

(defun ps-code-equal (template code)
  (cond
    ((and (consp template) (consp code))
     (and (= (length template) (length code))
          (every #'ps-code-equal template code)))
    ((and (symbolp template) (symbolp code))
     (ps-symbol-equal template code))
    (t
     (equal template code))))

#+5am
(let* ((a (gensym "A"))
       (*ps-placeholder-map* `((,a . b))))
  (assert (ps-code-equal `(+ 1 ,a) '(+ 1 b)))
  (assert (not (ps-code-equal `(+ 1 ,a) '(+ 1 c)))))

(defun ps= (template code)
  "Check whether Parenscript CODE matches TEMPLATE.

CODE matches template if symbol names are the same even if they are different objects, i.e. from different packages.

If symbol in TEMPLATE starts with underscore (_) it does not have to have the same name in CODE, but only be in same places as symbol it is being compared with."
  (let ((*ps-placeholder-map* nil))
    (ps-code-equal template code)))

