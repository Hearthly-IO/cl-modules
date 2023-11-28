(defpackage cl-modules
  (:use :cl)
  (:export :module :functor :call-functor))

(in-package :cl-modules)

(defmacro module (module-name &body body)
  "Define a module by prefixing module name to various definitions and internal references."
  (labels
    ((def-forms () '(defun defconstant defvar defparameter defgeneric defmethod module))
     (collect-module-names ()
       (loop for form in body
             when (eq (first form) 'module)
             collect (second form)))
     ;; Function to recursively collect names, including those in nested modules
     (collect-def-names (body)
       (mapcan (lambda (form)
                 (unless (eq (first form) 'module)
                   (mapcar (lambda (form) (when (member (first form) (def-forms)) (second form)))
                           body)))
               body))

     (module-prefix-p (symbol module-name)
       (let* ((symbol-name (symbol-name symbol))
              (prefix (format nil "~a-" module-name))
              (prefix-length (length prefix)))
         (and (>= (length symbol-name) prefix-length)
              (string-equal (subseq symbol-name 0 prefix-length) prefix))))

     (some-module-prefix-p (symbol)
       (some (lambda (module-name) (module-prefix-p symbol module-name))
             (collect-module-names)))

     ;; Collect names from the top-level body
     (def-names () (collect-def-names body))
     (prefix-name (form)
       (intern (format nil "~A-~A" module-name form)))
     (prefix-references (form)
       (if (atom form)
           (if (or (member form (def-names) :test 'equal)
                   (and (symbolp form) (some-module-prefix-p form)))
               (prefix-name form)
               form)
           (mapcar #'prefix-references form))))

    ;; Transform the body, including nested modules
    `(progn
       ,@(loop for form in body collect
           (cond
             ;; Handle module and def- forms by prefixing the variable name
             ((member (first form) (def-forms))
              `(,(first form) ,(prefix-name (second form)) ,@(mapcar #'prefix-references (cddr form))))
             ;; Pass through any other forms unmodified
             (t (prefix-references form)))))))


(defmacro functor (name args &body body)
  "Define a functor template."
  `(defconstant ,name (list ',args ',body)))


(defmacro call-functor (functor-name &rest module-args)
  "Instantiate a functor with actual module arguments."
  (labels
    ;; Function to find a replacement for a given module placeholder name.
    ((find-replacement (symbol mapping)
       (if (symbolp symbol)
           (let ((symbol-name (symbol-name symbol)))
             (dolist (pair mapping symbol)  ; 'symbol' as the return value if no match is found
               (let* ((prefix (symbol-name (car pair)))
                      (prefix-length (length prefix)))
                 (when (and (>= (length symbol-name) prefix-length)
                            (string-equal (subseq symbol-name 0 prefix-length) prefix))
                   (return (intern (concatenate 'string (symbol-name (cdr pair)) "-"
                                                (subseq symbol-name (1+ prefix-length)))))))))
           symbol))
     (process-sublist (sublist mapping)
       (mapcar (lambda (item)
                 (cond ((symbolp item) (or (find-replacement item mapping) item))
                       ((listp item) (process-sublist item mapping))
                       (t item)))
               sublist))
     (is-valid-module-list-p (element)
       (and (listp element)
            (>= (length element) 3)
            (eq (first element) 'module)
            (symbolp (second element))))
     (check-arguments (lst)
       (every (lambda (item)
                (or (symbolp item)
                    (is-valid-module-list-p item)))
              lst)))

    ;; Extract functor data from the named constant.
    (let* ((functor-data (symbol-value functor-name))
           (arg-names (first functor-data))     ; Extract argument names from the functor template
           (functor-body (second functor-data)) ; Extract the body of the functor template
           ;; Collect module definitions and names
           (module-defs
             (remove-if-not (lambda (arg) (and (listp arg) (eq (first arg) 'module))) module-args))
           (module-names
             (mapcar (lambda (arg) (if (listp arg) (second arg) (intern (symbol-name arg)))) module-args))
           ;; Create a mapping from placeholder names to actual module names
           (name-map (pairlis arg-names module-names)))

      (unless (= (length arg-names) (length name-map))
        (error "Mapping length does not match the module arg length."))

      (unless (check-arguments module-args)
        (error "Module args must either be a symbol or a list of pairs of symbols"))

      ;; Generate the module with the transformed functor body and any inline module definitions
      `(module ,functor-name
         ,@module-defs
         ,@(mapcar (lambda (sub-form) (process-sublist sub-form name-map)) functor-body)))))
