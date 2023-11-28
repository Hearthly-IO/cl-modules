# cl-modules

Ocaml-style modules for common lisp 


## Example

Modules:

```lisp
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
```

And functors:

```lisp
(module arithmetic
  (defun add (x y) (+ x y))
  (defun sub (x y) (- x y)))

(call-functor example arithmetic
  ;; You can pass in a module inline as well
  (module str
    (defun cat (x y) (format nil "~a~a" x y))))
```

See `test-cl-modules.lisp` for more on how these examples work.


## License

See LICENSE
