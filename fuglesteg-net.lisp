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

;;; Articles

;; TODO: Write document superclass??
;; use it to implement /projects

(defclass article ()
  ((file-path
    :reader file-path
    :initarg :file-path
    :type pathname)
   (content
    :accessor content
    :type string
    :initform "")
   (title
    :accessor title)
   (first-paragraph
    :reader first-paragraph
    :type string
    :initform "")
   (created-date
    :accessor created-date
    :initform "Not specified"
    :type string)))

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

(defmethod load-file ((article article))
  (let ((doc (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown) (uiop:read-file-string (file-path article)))))
    (with-slots (title content first-paragraph created-date) article
      (setf title (find-title doc)
            content (common-doc.format:emit-to-string (make-instance 'common-html:html) doc)
            first-paragraph (find-first-paragraph doc)
            created-date (common-doc:get-meta doc "date")))))

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

(defmethod initialize-instance :after ((article article) &key)
  (load-file article))

(defclass article-list ()
  ((directory
    :initarg :directory
    :type pathname)
   (articles
    :reader article-list-articles
    :type list)))

(defmethod populate-article-list ((article-list article-list))
  (with-slots (directory articles) article-list
    (let ((files (uiop/filesystem:directory-files directory "*.md")))
      (setf articles (loop for file in files
            collect (make-instance 'article :file-path file))))))

(defmethod initialize-instance :after ((article-list article-list) &key)
  (populate-article-list article-list))

(defvar *articles-list* (make-instance 'article-list :directory #P"./articles"))

(comment
  (notify:watch #P"./articles")

  (notify:with-events (file change :timeout t)
    (populate-article-list *articles-list*)))

(sb-thread:make-thread (lambda ()
                         (notify:with-events (file change :timeout t)
                           (populate-article-list *articles-list*))))

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

(defmacro arrow-stylesheet ()
  (uiop:read-file-string #P"./arrow.css"))

(deftag arrow (body attrs &key link content)
  `(progn (:style ,(arrow-stylesheet))
          (:div :class "arrow-container"
           (:a :href ,link
            (:div :class "long-arrow-left") 
            (:p ,content)))))

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

(defmacro lorem-ipsum ()
  (quote " Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper  iaculis est, id viverra elit ultrices a. In tempor id nisl at volutpat.  Aenean quis consequat neque. Nullam interdum consectetur odio quis  condimentum. Quisque laoreet nisl semper turpis consequat hendrerit.  Cras scelerisque suscipit pharetra. Donec efficitur dolor quis venenatis  scelerisque. Sed pulvinar maximus risus, eget vehicula mi venenatis id.
 Phasellus turpis dui, elementum vel eleifend at, convallis ut augue.  Nullam eu odio odio. Vestibulum a erat fringilla, convallis mi at,  maximus lectus. Suspendisse luctus velit id turpis venenatis sodales.  Integer lobortis quam tortor. Pellentesque ac luctus leo, tincidunt  scelerisque urna. Cras nec tempus ex. Fusce feugiat ultricies est, id  elementum quam. Nulla facilisi. Nam efficitur, odio sed convallis  vehicula, massa eros fringilla sapien, vel pharetra neque augue rutrum  libero. Vivamus iaculis ex ut cursus scelerisque. Donec iaculis aliquam  velit, ac fermentum quam ornare ac. Vivamus sit amet ex a arcu tincidunt  dignissim sit amet non justo. Vivamus a orci risus.
 Vivamus at massa est. Mauris efficitur leo eu mauris tristique  tincidunt. Suspendisse eu luctus sapien. Aenean urna sapien, gravida sed  leo malesuada, tincidunt convallis quam. Donec sagittis magna sit amet  lorem laoreet cursus. Etiam bibendum ultricies enim sed molestie. Aenean  sit amet finibus sapien, et efficitur justo. Donec facilisis est ac  nulla semper, et tempus mauris fermentum. Nullam egestas consequat ante,  sit amet dapibus magna suscipit sed. Etiam at dui pellentesque,  facilisis urna ac, ullamcorper arcu.
 Cras egestas libero non ipsum dictum, vel dignissim urna lacinia. Ut  dictum eleifend nulla at rhoncus. Nunc gravida lectus nec ante tristique  placerat. Mauris ac lectus efficitur, cursus ex eget, sodales ex.  Praesent sodales ante euismod, volutpat mauris nec, venenatis est. In  vel mattis elit, sit amet auctor enim. Suspendisse ut velit in odio  faucibus convallis. Fusce neque sapien, mattis et iaculis vel, ultricies  et purus.
 Morbi mollis mauris sit amet augue pulvinar cursus. Vivamus vitae justo  porta, molestie ante non, pellentesque eros. Nullam quis placerat metus.  Suspendisse diam purus, pellentesque non nisl eu, eleifend aliquam  ipsum. Curabitur accumsan, est vitae elementum mollis, lorem felis  elementum felis, et tristique nulla ipsum vitae orci. Phasellus aliquet,  felis et venenatis pretium, lacus nulla sollicitudin massa, non rhoncus  enim lacus eget dolor. Fusce eget velit purus. Aliquam maximus congue  velit, in aliquam sapien fringilla aliquet. Vestibulum ligula lorem,  hendrerit vitae consectetur vitae, convallis ac nunc. Nulla facilisi."))

(deftag article-thumbnail (body attrs &key article)
  `(:a :href (concatenate 'string "/articles/" (title ,article))
    (:div :class "article-thumb"
     (:p (created-date ,article))
     (:h3 (title ,article))
     (:p (with-slots (first-paragraph) article
           (concatenate 'string 
                        (subseq first-paragraph 0 (min (length first-paragraph) 100))
                        "..."))))))

(defmacro articles-style-sheet ()
  (uiop:read-file-string #P"./articles.css"))

(defmacro defpage (name route title &body body)
  `(defroute ,name ,route
     `(200 (:content-type "text/html")
           (,(base-html ,title
               ,@body)))))

(deftag articles-list (body attrs)
  `(progn (:style (:raw ,(articles-style-sheet)))
          (:div :class "article-thumb-container"
           `(progn ,(loop for article in (article-list-articles *articles-list*)
                          collect (article-thumbnail :article article))))))

(deftag contact (body attrs)
  `(progn
     (:h2 "Find me on:")
     (:a :href "https://linkedin.com/in/andreas-fuglesteg-dale" (:img :style "width: 4rem" :src "/public/linkedin.svg"))
     (:a :href "https://github.com/Fuglesteg" (:img :style "width: 4rem" :src "/public/github.svg"))))

(defpage index "/" "Fuglesteg.net"
  (:p "Hello! Welcome to my corner of the internet." 
   (:br) 
   "Here I write about my coding projects and other things that I find interesting."
   (:br)
   (:a :class "link" :href "/articles/fuglesteg.net" "Read about how this page was made"))
  (:br)
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
                      (article-list-articles *articles-list*))))
        (if article
            (progn
              (:script (:raw (ps:ps* `(setf (ps:@ document title) ,title))))
              (:p (created-date article))
              (:raw (content article)))
            (signal 'not-found))))

; TODO
(defvar *about-article* (make-instance 'article :file-path #P"./about-me.md"))
  
(defpage about "/about" "About me"
  (:raw (content *about-article*)))

(defpage projects "/projects" "Projects"
  (:p "hello"))

(defun not-found ()
  `(404 (:content-type "text/html")
        (,(base-html "404 - Not Found"
            (:p "The page could not be found ;(")))))
