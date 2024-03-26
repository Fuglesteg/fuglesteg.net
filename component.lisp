(defpackage :component
  (:use :cl))

(in-package :component)

#+comp 
(defcomponent counter (label &key initial)
  (:p ,label)
  (:p (var value))
  (:button :onclick increment "Increment")
  (:script (defun increment () (incf value))))

#+comp 
(defcomponent counter (label &key initial)
  (:p ,label)
  (:p (bind (concat "value: " value)))
  (:button :onclick (incf value) "Increment"))

(defmacro defcomponent (name props &body body)
  `(ps ,@(loop for element in body collect (generate-update-statements element))))

;;(defcomponent hello '() (:p (var text)) (:button :onclick (incf text)))

(defvar test% '((:a :href (var (+ 1 path)) (var name)) (:p (var text))))

(defun varp (list)
  (and (listp list)
       (eq (car list) 'var)))

(defun generate-update-statement (element-id var)
  `(setf 
    (chain document (get-element-by-id ,element-id) 
           ,(intern (symbol-name (car var)))) ; Used to turn keywords into symbols, improves javascript output
    value))

;(defmethod add-update-statement ((var js-variable) update-statement)
;(setf (update-statements var) (append update-statement (update-statements var))))

;;(group-update-statements (generate-update-statements (first test%)))

;;(generate-update-statements (first test%))

(defun group-update-statements (update-statements)
  (loop for statement in update-statements
        for var-name = (car statement)
        with vars = '()
        do (if (assoc var-name vars)
               (setf (cdr (assoc var-name vars)) (cdr statement))
               (setf vars (append `(,(car statement) . ,(list (cdr statement))) vars)))
        finally (return vars)))

(defun generate-update-statements (element)
  (with-ps-gensyms (element-id)
    (let ((var-forms (collect-vars-in-element element))
          (element-id (string element-id)))
      (mapcar (lambda (var) (cons (car (cdadr var)) (list(generate-update-statement element-id var)))) var-forms))))

(defun collect-vars-in-element (element)
  (loop for keyword in element
        for i from 1
        with last-keyword = keyword
        when (varp keyword)
        collect (list 
                 (if (< i (length element)) 
                     last-keyword 
                     :inner-text)
                 keyword)
        do (setf last-keyword keyword)))
