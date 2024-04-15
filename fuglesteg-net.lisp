(ql:quickload :spinneret)
(asdf:load-system :spinneret/ps)
(ql:quickload :parenscript)
(ql:quickload :clack)
(ql:quickload :lack)
(ql:quickload :serapeum)
(ql:quickload :alexandria)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defpackage fuglesteg.net
  (:use :cl :spinneret :alexandria :serapeum :parenscript))

(in-package #:fuglesteg.net)

(deftag counter (label attrs &key initial)
  (ps:with-ps-gensyms (value increment el-id)
    (let ((el-id (string el-id)))
      `(progn 
         (:p ,label)
         (:p :id ,el-id ,(or initial 0))
         (:p)
         (:input :type "button" :onclick (ps:ps (,increment)) :value "Increment")
         (:script :type "text/javascript" 
          (:raw (ps:ps
                  (let ((,value ,(or initial 0)))
                    (defun ,increment () 
                      (incf ,value)
                      (setf (ps:chain (ps:chain document (get-element-by-id ,el-id)) inner-text) ,value))))))))))

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

(defclass article ()
  ((file-path
    :reader file-path
    :initarg :file-path
    :type pathname)
   (content
    :accessor content
    :type string
    :initarg :content
    :initform "")
   (title
    :accessor title
    :initarg :title
    :type string)
   (created-date
    :accessor created-date
    :initarg :created-date
    :type string)))

(defvar *articles* (list
                    (make-instance 'article 
                                   :file-path #P"./timid.md"
                                   :title "Timid"
                                   :content (lorem-ipsum)
                                   :created-date "12.03.2023")
                    (make-instance 'article
                                   :file-path #P"./slither.md"
                                   :title "Slither"
                                   :content (lorem-ipsum)
                                   :created-date "14.03.2024")))

(deftag article-thumbnail (body attrs &key article)
  `(:a :href (concatenate 'string "/articles/" (title ,article))
    (:div :class "article-thumb"
     (:p (created-date ,article))
     (:h3 (title ,article))
     (:p (with-slots (content) article
           (concatenate 'string 
                        (subseq content 0 (min (length content) 100))
                        "..."))))))

(defmacro articles-style-sheet ()
  (uiop:read-file-string #P"./articles.css"))

(defun favicon (env)
  `(301 (:location "/public/favicon.ico")))

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
        (destructuring-bind (&optional route . route-tree) (find-route segment routes)
          (if (null route)
              (replace-route (make-route route-segments handler) routes)
              (replace-route `(,route ,@(merge-route rest-of-segments handler route-tree)) routes))))))

(defun replace-route (route routes)
  (if (not (listp routes))
      routes
      `(,@(remove (car route) routes :key #'car :test #'equal) ,route)))

(eval-when (:compile-toplevel)
  (defun register-route (route-segments handler)
    (setf *routes* (merge-route route-segments handler *routes*))))

(defmacro arrow-stylesheet ()
  (uiop:read-file-string #P"./arrow.css"))

(deftag arrow (body attrs &key link content)
  `(progn (:style ,(arrow-stylesheet))
          (:div :class "arrow-container"
           (:a :href ,link
            (:div :class "long-arrow-left") 
            (:p ,content)))))

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

(defmacro defpage (name route title &body body)
  `(defroute ,name ,route
         `(200 (:content-type "text/html")
               (,(base-html ,title
                   ,@body)))))

(defpage article "/articles/{title}" nil
  (arrow :link "/articles" :content "Articles")
  (let ((article (find-if 
                      (lambda (article)
                        (string=
                         (string-upcase (title article)) 
                         (string-upcase title))) 
                      *articles*)))
        (if article
            (progn (:h1 (title article))
                   (:p (content article)))
            (signal 'not-found))))

(defpage index "/" "Fuglesteg.net"
  (:a :href "/about" "About me")
  (counter "counter" :initial 2)
  (counter "counter2" :initial 34))

(defpage articles "/articles" "Articles"
  (:style (:raw (articles-style-sheet)))
  (:div :class "article-thumb-container"
   (progn (loop for article in *articles*
                collect (article-thumbnail :article article)))))

(defpage about "/about" "About me"
  (:p "Hi my name is Andreas Fuglesteg Dale. Nice to meet you!")
  (:p (lorem-ipsum)))

(defun not-found ()
  `(404 (:content-type "text/html")
        (,(base-html "404 - Not Found"))))
