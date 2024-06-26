(defpackage fuglesteg.net/routing
  (:use :cl :serapeum :alexandria)
  (:export :route-segments
           :route-parameter-p
           :register-route
           :defroute
           :dispatch
           :http-not-found))

(in-package :fuglesteg.net/routing)

(defvar *routes* '())

(defvar *env* '())

(defun route-segments (route)
  (split-sequence #\/ route :remove-empty-subseqs t))

(defun route-parameter-p (route-parameter)
  (and (char= (char route-parameter 0) #\{)
       (char= (char route-parameter (1- (length route-parameter))) #\})))

(defun route-parameter->symbol (route-parameter)
  (intern (string-upcase (subseq route-parameter 1 (1- (length route-parameter))))))

(defun find-handler (route-segment-list routes)
  (let* ((segment (car route-segment-list))
         (found (let ((route (find-route segment routes)))
                  (if (symbolp route)
                      route
                      (cdr route)))))
    (typecase found
      (symbol found)
      (list (find-handler (cdr route-segment-list) found)))))

(defun find-route (route-segment routes)
  (if (null route-segment)
      (assoc :root routes)
      (let ((named-route (assoc route-segment routes :test #'string=)))
        (or named-route 
            (assoc :parameter routes)))))

(defun make-route (route-segments handler)
  (if (null route-segments)
      `(:root . ,handler)
      (destructuring-bind (segment &optional . rest) route-segments
        `(,(if (route-parameter-p segment)
               :parameter segment) 
          . ,(if rest 
                 `(,(make-route rest handler))
                 handler)))))

(defun merge-route (route-segments handler routes)
  (destructuring-bind (&optional segment . rest-of-segments) route-segments
    (if (null segment)
        (replace-route (make-route route-segments handler) routes)
        (if (symbolp routes)
            `((:root . ,routes) ,(make-route route-segments handler))
            (destructuring-bind (&optional route . route-tree) (find-route segment routes)
              (if (null route)
                  (replace-route (make-route route-segments handler) routes)
                  (replace-route `(,route ,@(merge-route rest-of-segments handler route-tree)) routes)))))))

(defun replace-route (route routes)
  (if (not (listp routes))
      routes
      `(,@(remove (car route) routes :key #'car :test #'equal) ,route)))

(defun register-route (route-segments handler)
  (setf *routes* (merge-route route-segments handler *routes*)))

(defmacro defroute (name route &body body)
  (let* ((segments (route-segments route))
         (route-parameters (loop for segment in segments
                                 for i from 0
                                 when (route-parameter-p segment)
                                 collect (cons i (route-parameter->symbol segment)))))
    `(progn 
       (defun ,name ()
         (let ,(loop for (index . parameter) in route-parameters
                     collect `(,parameter (nth ,index (getf *env* :route-segments))))
           ,@body))
       (register-route ',segments ',name))))

(defcondition http-not-found () ())

(defun dispatch (env)
  (let* ((path (getf env :path-info))
         (segments (route-segments path))
         (handler (find-handler segments *routes*)))
    (if (or (null handler) (not (fboundp handler)))
        (signal 'http-not-found)
        (let ((*env* env))
          (setf (getf *env* :route-segments) segments)
          (funcall handler)))))

