(defpackage fuglesteg.net/document
  (:use :cl :alexandria :serapeum)
  (:export :document
           :article
           :project
           :document-list
           :document-list-documents
           :title
           :content
           :technologies
           :name
           :link
           :source-link
           :created-date
           :synopsis
           :images
           :populate-document-list))

(in-package :fuglesteg.net/document)

(defmethod common-doc.format:parse-document :around ((markdown commondoc-markdown:markdown) (input string))
  "Parse yaml frontmatter and add it to the metadata of the returned node"
  (let ((lines (split-sequence #\Newline input)))
    (destructuring-bind (first-line . lines) lines
      (if (not (string= first-line "---"))
          (call-next-method)
          (let* ((yaml-lines (loop for line in lines
                                   for i from 1
                                   until (string= line "---")
                                   collect line
                                   finally (setf lines (subseq lines i))))
                 (yaml-string (string-join yaml-lines #\Newline))
                 (yaml (yaml:parse yaml-string))
                 (input (string-join lines #\Newline))
                 (doc (call-next-method markdown input)))
            (setf (common-doc:metadata doc) yaml)
            doc)))))

(defclass document ()
  ((file-path
    :reader file-path
    :initarg :file-path
    :type pathname)
   (content
    :accessor content
    :documentation "The parsed HTML content of the document"
    :type string
    :initform "")))

(defmethod load-file ((document document))
  (let ((common-doc (common-doc.format:parse-document 
                     (make-instance 'commondoc-markdown:markdown)
                     (uiop:read-file-string (file-path document)))))
    (setf (content document) (common-doc.format:emit-to-string (make-instance 'common-html:html) common-doc))
    (populate-data document common-doc)))

(defmethod initialize-instance :after ((document document) &key)
  (load-file document))

(defmethod populate-data ((document document) (common-doc common-doc:content-node)))

(defmethod document-sort-value ((document document))
  0)

(defclass thumbnail ()
  ((synopsis
    :accessor synopsis
    :type string
    :initform "")))

(defmethod populate-data :before ((thumbnail thumbnail) (common-doc common-doc:content-node))
  (setf (synopsis thumbnail) (find-first-paragraph common-doc)))

(defclass article (document thumbnail)
  ((title
    :accessor title)
   (created-date
    :accessor created-date
    :initform "Not specified"
    :type string)))

(defun parse-date (date)
  "Parse a date in format dd.MM.YYYY into a number for sorting"
  (destructuring-bind (day month year) (mapcar #'parse-integer
                                               (uiop:split-string date :separator '(#\.)))
    (+ (* year 10000)
       (* month 100)
       day)))

(defun date< (&rest dates)
  (apply #'< (mapcar #'parse-date dates)))

(defun date> (&rest dates)
  (apply #'> (mapcar #'parse-date dates)))

(defmethod document-sort-value ((article article))
  (parse-date (created-date article)))

(defmethod populate-data ((article article) (common-doc common-doc:content-node))
  (with-slots (title first-paragraph created-date) article
    (setf title (find-title common-doc)
          created-date (or (common-doc:get-meta common-doc "date") created-date))))

(defclass project (document thumbnail)
  ((name
    :accessor name)
   (link
    :accessor link)
   (source-link
    :accessor source-link)
   (technologies
    :accessor technologies)
   (images
    :accessor images)
   (date
    :accessor project-date)))

(defmethod populate-data ((project project) (common-doc common-doc:content-node))
  (with-slots (name link source-link technologies images date) project
    (setf name (common-doc:get-meta common-doc "name")
          link (common-doc:get-meta common-doc "link")
          source-link (common-doc:get-meta common-doc "source-link")
          technologies (common-doc:get-meta common-doc "technologies")
          date (or (common-doc:get-meta common-doc "date") "0.0.0")
          images (find-images project))))

(defmethod find-images ((project project))
  (let* ((image-directory-absolute-uri (format nil "/public/projects/~a" (pathname-name (file-path project))))
         (image-directory (pathname (format nil ".~a" image-directory-absolute-uri))))
    (mapcar (lambda (path)
              (format nil "~a/~a.~a" image-directory-absolute-uri (pathname-name path) (pathname-type path)))
            (uiop/filesystem:directory-files image-directory))))

(defmethod document-sort-value ((project project))
  (parse-date (project-date project)))

(defclass document-list ()
  ((directory
    :initarg :directory
    :type pathname)
   (type
    :initarg :type
    :initform 'document
    :type symbol)
   (documents
    :reader document-list-documents
    :type list)))

(defmethod populate-document-list ((document-list document-list))
  (with-slots (directory documents type) document-list
    (let ((files (uiop/filesystem:directory-files directory "*.md")))
      (setf documents (sort (loop for file in files
                                  collect (make-instance type :file-path file))
                            (lambda (document1 document2)
                              (> (document-sort-value document1)
                                 (document-sort-value document2))))))))

(defmethod initialize-instance :after ((document-list document-list) &key)
  (populate-document-list document-list))

(defmethod find-title ((node common-doc:content-node))
  (find-title (common-doc:children node)))

(defmethod find-title ((node common-doc:section))
  (find-title (common-doc:title node)))

(defmethod find-title ((node common-doc:text-node))
  (common-doc:text node))

(defmethod find-title ((children list))
  (if (null children)
      '()
      (destructuring-bind (first-child . rest) children
        (let ((text (find-title first-child)))
          (if (stringp text)
              text
              (find-title rest))))))

(defmethod find-title ((node t))
  nil)

(defmethod find-first-paragraph ((children list))
  (if (null children)
      '()
      (destructuring-bind (first-child . rest) children
        (let ((text (find-first-paragraph first-child)))
          (if (stringp text)
              text
              (find-first-paragraph rest))))))

(defmethod find-first-paragraph ((node common-doc:content-node))
  (find-first-paragraph (common-doc:children node)))

(defmethod find-first-paragraph ((node common-doc:text-node))
  (common-doc:text node))

(defmethod find-first-paragraph ((node t))
  nil)

