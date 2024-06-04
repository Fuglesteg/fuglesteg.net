(ql:quickload :spinneret)
(asdf:load-system :spinneret/ps)
(ql:quickload :parenscript)
(ql:quickload :clack)
(ql:quickload :lack)
(ql:quickload :serapeum)
(ql:quickload :alexandria)
(ql:quickload :common-doc)
(ql:quickload :commondoc-markdown)
(ql:quickload :common-html)
(ql:quickload :file-notify)
(asdf:load-system :cl-yaml)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defpackage fuglesteg.net
  (:use :cl :spinneret :alexandria :serapeum :parenscript)
  (:local-nicknames (#:notify #:org.shirakumo.file-notify)))

(in-package #:fuglesteg.net)

;;; Documents (Articles & Projects)

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
    :type string
    :initform "")))

(defmethod load-file ((document document))
  (let ((common-doc (common-doc.format:parse-document 
                     (make-instance 'commondoc-markdown:markdown)
                     (uiop:read-file-string (file-path document)))))
    (setf (content document) (common-doc.format:emit-to-string (make-instance 'common-html:html) common-doc))
    (populate-data document common-doc)))

(defmethod populate-data ((document document) (common-doc common-doc:content-node)))

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
    :accessor images)))

(defmethod populate-data ((project project) (common-doc common-doc:content-node))
  (with-slots (name link source-link technologies images) project
    (setf name (common-doc:get-meta common-doc "name")
          link (common-doc:get-meta common-doc "link")
          source-link (common-doc:get-meta common-doc "source-link")
          technologies (common-doc:get-meta common-doc "technologies")
          images (find-images project))))

(defmethod find-images ((project project))
  (let* ((image-directory-absolute-uri (format nil "/public/projects/~a" (pathname-name (file-path project))))
         (image-directory (pathname (format nil ".~a" image-directory-absolute-uri))))
    (mapcar (lambda (path)
              (format nil "~a/~a.~a" image-directory-absolute-uri (pathname-name path) (pathname-type path)))
            (uiop/filesystem:directory-files image-directory))))

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
      (setf documents (loop for file in files
                            collect (make-instance type :file-path file))))))

(defmethod initialize-instance :after ((document-list document-list) &key)
  (populate-document-list document-list))

(defmethod initialize-instance :after ((document document) &key)
  (load-file document))

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

(defvar *articles-list* (make-instance 'document-list :type 'article :directory #P"./articles"))

(comment
  (notify:watch #P"./articles")

  (notify:with-events (file change :timeout t)
    (populate-document-list *articles-list*)))

(sb-thread:make-thread (lambda ()
                         (notify:with-events (file change :timeout t)
                           (populate-document-list *articles-list*))))

;;; Routing

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

(defvar *routes* '())

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

(eval-always
  (defun register-route (route-segments handler)
    (setf *routes* (merge-route route-segments handler *routes*))))

(defmacro defroute (name route &body body)
  (let* ((segments (route-segments route))
         (route-parameters (loop for segment in segments
                                 for i from 0
                                 when (route-parameter-p segment)
                                 collect (cons i (route-parameter->symbol segment)))))
    (register-route segments name)
    `(defun ,name ()
       (let ,(loop for (index . parameter) in route-parameters
                   collect `(,parameter (nth ,index (getf *env* :route-segments))))
         ,@body))))

;;; Application

(defvar *env* '())

(defcondition not-found () ())

(defun dispatch (env)
  (let* ((path (getf env :path-info))
         (segments (route-segments path))
         (handler (find-handler segments *routes*)))
    (if (or (null handler) (not (fboundp handler)))
        (not-found)
        (let ((*env* env))
          (setf (getf *env* :route-segments) segments)
          (handler-case (funcall handler)
            (not-found () (not-found)))))))

(defvar *app*
  (lack:builder
   (:static :path "/public/"
    :root #P"./public/")
   (lambda (env) (dispatch env))))

(defvar *handler* nil)

(defun start ()
  (when *handler*
    (clack:stop *handler*))
  (setf *handler* (clack:clackup *app*)))

;;; Markup

;; Close tags
(setf *html-style* :tree)

(defmacro arrow-stylesheet ()
  (uiop:read-file-string #P"./arrow.css"))

(deftag arrow (body attrs &key link content)
  `(progn (:style ,(arrow-stylesheet))
          (:a :class "arrow-container" :href ,link
           (:div :class "long-arrow-left") 
           (:p ,content))))

(deftag header (body attrs)
  `(progn
     (:header :class "header"
       (:a :href "/"
        (:img :class "logo" :alt "Fuglesteg logo" :src "/public/logo.svg"))
       (:div
        (:a :href "/"
         (:h1 :class "header-title" "Fuglesteg"))
        (:nav :class "header-nav"
         (:a :class "button" :href "/about" "About me")
         (:a :class "button" :href "/articles" "Articles")
         (:a :class "button" :href "/projects" "Projects"))))))

(defmacro style-sheet ()
  (uiop:read-file-string #P"./style.css"))

(defmacro base-html (title &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:link :rel "preconnect" :href "https://fonts.googleapis.com")
       (:link :rel "preconnect" :href "https://fonts.gstatic.com")
       (:raw "<link rel=\"preload\" href=\"https://fonts.googleapis.com/css2?family=Marcellus&display=swap\" as=\"style\">")
       (:link :href "https://fonts.googleapis.com/css2?family=Marcellus&display=swap" :rel "stylesheet")
       (:link :rel "icon" :href "/public/favicon.ico")
       (:title ,title)
       (:style (:raw (style-sheet))))
      (:body
       (header)
       (:div :class "content"
        ,(when title `(:h1 ,title))
        ,@body)))))

(defmacro defpage (name route title &body body)
  `(defroute ,name ,route
     `(200 (:content-type "text/html")
           (,(base-html ,title
               ,@body)))))

(defmacro articles-style-sheet ()
  (uiop:read-file-string #P"./articles.css"))

(deftag article-thumbnail (body attrs &key article)
  `(:a :href (concatenate 'string "/articles/" (title ,article))
    (:div :class "thumbnail"
     (:p (created-date ,article))
     (:h3 (title ,article))
     (:p (with-slots (synopsis) article
           (concatenate 'string 
                        (subseq synopsis 0 (min (length synopsis) 100))
                        "..."))))))

(deftag articles-list (body attrs)
  `(progn (:style (:raw ,(articles-style-sheet)))
          (:div :class "grid-container"
           (loop for article in (document-list-documents *articles-list*)
                 collect (article-thumbnail :article article)))))

(deftag contact (body attrs)
  `(progn
     (:h2 "Find me on:")
     (:a :href "https://linkedin.com/in/andreas-fuglesteg-dale" (:img :style "width: 4rem" :src "/public/icons/linkedin.svg"))
     (:a :href "https://github.com/Fuglesteg" (:img :style "width: 4rem" :src "/public/icons/github.svg"))))

(defpage index "/" "Fuglesteg.net"
  (:p "Hello! Welcome to my corner of the internet." 
   (:br) 
   "Here I write about my coding projects and other things that I find interesting."
   (:br)
   (:a :class "link" :href "/articles/fuglesteg.net" "Read about how this page was made"))
  (contact)
  (:hr)
  (:h2 "Recent articles")
  (articles-list))

(defpage articles "/articles" "Articles"
  (articles-list))

(defpage article "/articles/{title}" nil
  (arrow :link "/articles" :content "Articles")
  (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/tomorrow-night-bright.min.css")
  (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js")
  (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/go.min.js")
  (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/lisp.min.js")
  (:script (:raw (ps (chain hljs (highlight-all)))))
  (let ((article (find-if 
                  (lambda (article)
                    (string=
                     (string-upcase (title article))
                     (string-upcase title))) 
                  (document-list-documents *articles-list*))))
    (if article
        (progn
          (:script (:raw (ps:ps* `(setf (ps:@ document title) ,title))))
          (:p (created-date article))
          (:raw (content article)))
        (signal 'not-found))))

(defvar *about-document* (make-instance 'document :file-path #P"./about-me.md"))

(defpage about "/about" "About me"
  (:raw (content *about-document*)))

(defvar *devicon-overrides*
  '("lisp" "guix" "github"))

(deftag devicon (body attrs &key name)
  `(let ((src (if (member ,name *devicon-overrides* :test #'string=)
                  (format nil "/public/icons/~a.svg" ,name)
                  (format nil "https://cdn.jsdelivr.net/gh/devicons/devicon@latest/icons/~a/~:*~a-original.svg" ,name))))
     (:img 
      :style "width: 10%;"
      :src src)))

(deftag project-thumbnail (body attrs &key project)
  `(:a :href (format nil "/projects/~a" (name ,project))
    (:div :class "thumbnail"
     (:h2 (name ,project))
     (:p (synopsis ,project))
     (:div (loop for technology in (technologies ,project)
                 collect (devicon :name technology))))))

(defvar *projects-list* (make-instance 'document-list :type 'project :directory #P"./projects"))

(deftag project-list (body attrs)
  `(progn (:style (:raw ,(articles-style-sheet)))
          (:div :class "grid-container"
           (loop for project in (document-list-documents *projects-list*)
                           collect (project-thumbnail :project project)))))

(defpage projects "/projects" "Projects"
  (project-list))

(defmacro repeating-gallery-style-sheet ()
  (uiop:read-file-string #P"./gallery.css"))

(deftag repeating-gallery (body attrs &key images)
  (ps:with-ps-gensyms (gallery-container)
    `(progn
       (:style (:raw ,(repeating-gallery-style-sheet)))
       (:script :type "module"
        (:raw (ps:ps
                (ps:chain ,gallery-container
                          (add-event-listener "wheel" 
                                              (lambda (e)
                                                (ps:chain e (prevent-default))
                                                (let ((scrolling-right (> (ps:@ ,gallery-container scroll-left) 
                                                                          (- (ps:@ ,gallery-container scroll-left-max) 1000)))
                                                      (scrolling-left (< (ps:@ ,gallery-container scroll-left) 1000)))
                                                  (when (or scrolling-right scrolling-left)
                                                    (let* ((images (ps:chain ,gallery-container children))
                                                           (index (if scrolling-right 0 (1- (length images))))
                                                           (image (ps:aref images index)))
                                                      (ps:chain ,gallery-container 
                                                                (scroll-by 
                                                                 (ps:create
                                                                  left (if scrolling-right
                                                                           (- (ps:@ image width))
                                                                           (ps:@ image width))
                                                                  behavior "instant")))
                                                      (if scrolling-right
                                                          (ps:chain ,gallery-container (append image))
                                                          (ps:chain ,gallery-container (prepend image))))))
                                                (incf (ps:@ ,gallery-container scroll-left) (* 5 (ps:@ e delta-y)))))))))
       (:div :id ,(ps:symbol-to-js-string gallery-container)
        :class "gallery-container" 
        (loop for image in ,images
              do (:img :class "gallery-item" :src image))))))

(defpage project "/projects/{name}" nil
  (:style (:raw (articles-style-sheet)))
  (arrow :link "/projects" :content "Projects")
  (let ((project (find-if
                  (lambda (project)
                    (string=
                     (string-upcase (name project))
                     (string-upcase name)))
                  (document-list-documents *projects-list*))))
    (if project
        (progn
          (:script (:raw (ps:ps* `(setf (ps:@ document title) ,name))))
          (:raw (content project))
          (repeating-gallery :images (images project)))
        (signal 'not-found))))

(defun not-found ()
  `(404 (:content-type "text/html")
        (,(base-html "404 - Not Found"
            (:p "The page could not be found ;(")))))
