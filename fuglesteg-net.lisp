(ql:quickload :spinneret)
(asdf:load-system :spinneret/ps)
(ql:quickload :parenscript)
(ql:quickload :clack)

(defpackage fuglesteg.net
  (:use :cl :spinneret :parenscript))

(in-package #:fuglesteg.net)

(defvar *handler*
  (clack:clackup
   (lambda (env)
     (dispatch env))))

#+comp 
(defcomponent counter (label &key initial)
  (:p ,label)
  (:p (var value))
  (:button :onclick increment "Increment")
  (:script (defun increment () (incf value))))

#+comp 
(defcomponent counter (label &key initial)
  (:p ,label)
  (:p (var value))
  (:button :onclick (incf value) "Increment"))

(defmacro defcomponent (name props &body body)
  `(print ,@(loop for form in body when (getf form :p) collect it)))

(defvar test% '((:a :href "/" (var dest)) (:p (var text))))

(deftag counter (label attrs &key initial)
  (with-ps-gensyms (value increment el-id)
    (let ((el-id (string el-id)))
      `(progn 
         (:p ,label)
         (:p :id ,el-id ,(or initial 0))
         (:p)
         (:input :type "button" :onclick (ps (,increment)) :value "Increment")
         (:script :type "text/javascript" 
          (:raw (ps
                  (let ((,value ,(or initial 0)))
                    (defun ,increment () 
                      (incf ,value)
                      (setf (chain (chain document (get-element-by-id ,el-id)) inner-text) ,value))))))))))

(defun collect-forms-with-var (tree)
  (cond
    ((and (consp tree) (eq (car tree) 'var)) (list tree))
    ((consp tree)
     (append
      (collect-forms-with-var (car tree))
      (collect-forms-with-var (cdr tree))))))

(defmacro base-html (title &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body
       (:h1 ,title)
       ,@body))))

(defun index (env)
  `(200 (:content-type "text/html") 
        (,(base-html "Fuglesteg.net"
            (:script :type "text/javascript"
             (:raw (ps (defun test () (var message "hello") (alert message)))))
            (:a :href "/about" "About me")
            (counter "counter" :initial 2)
            (counter "counter2" :initial 34)))))

(defun not-found ()
  `(404 (:content-type "text/html")
        (,(base-html "404 - Not Found"))))

(defun about (env)
  `(200 (:content-type "text/html")
        (,(base-html "About me"
            (:p "Hi my name is Andreas Fuglesteg Dale. Nice to meet you!")))))

(defvar *routes*
  '(("/" . index)
    ("/about" . about)))

(defun dispatch (env)
  (let ((handler (cdr (assoc (getf env :path-info) *routes* :test #'string=))))
    (if (null handler)
        (not-found)
        (funcall handler env))))
