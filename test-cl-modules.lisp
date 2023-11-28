(defpackage test-cl-modules
  (:use :cl :1am :cl-modules))

(in-package :test-cl-modules)

;(pushnew (uiop:getcwd) asdf:*central-registry* :test #'equal)
;(pushnew (uiop:merge-pathnames* #P"lisp-systems/1am/" (uiop:getcwd)) asdf:*central-registry* :test #'equal)

(defun sub (x y) (- x y))

;; module

(module math
  (module constant
    (defconstant pi 3.14159))

  (defun add (x y) (+ x y))
  (defun inc (x) (add x 1))
  (defun dec (x) (sub x 1))
  (defun times-pi (x) (* x constant-pi))
  (defvar some-var 10)
  (defparameter some-param 20)
  (defgeneric compute (x))
  (defmethod compute ((x number)) (* x 2)))

(test module-test
  (is (= (math-add 2 3) 5))
  (is (= (math-inc 5) 6))
  (is (= (math-dec 5) 4))
  (is (= (math-times-pi 2) 6.28318))
  (is (= math-constant-pi 3.14159))
  (is (= math-some-var 10))
  (is (= math-some-param 20))
  (is (= (math-compute 5) 10)))


;; functor

(functor example (module-a module-b)
  (defun test () 
    (format nil "~A ~A" (module-a-add 1 2) (module-b-cat 5 3))))

(test functor-test
  (is (equalp example 
        '((module-a module-b)
          ((defun test ()
             (format nil "~a ~a" (module-a-add 1 2) (module-b-cat 5 3))))))))


;; call-functor

(module arithmetic
  (defun add (x y) (+ x y))
  (defun sub (x y) (- x y)))

(call-functor example arithmetic
  (module str
    (defun cat (x y) (format nil "~a~a" x y))))

(test call-functor-test
  (is (equal (example-test) "3 53")))

(run)
