(ql:quickload :spinneret)
(asdf:load-system :spinneret/ps)
(ql:quickload :parenscript)
(ql:quickload :clack)
(ql:quickload :lack)
(ql:quickload :serapeum)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defpackage fuglesteg.net
  (:use :cl :spinneret :parenscript))

(in-package #:fuglesteg.net)

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

(deftag header (body attrs)
  `(progn
     (:header :class "header"
       (:a :href "/" :class "header-title"
        (:img :class "logo" :alt "Fuglesteg logo" :src "/public/logo.svg")
        (:h1 "Fuglesteg"))
       (:nav :class "header-nav"
        (:a :class "button" :href "/about" "About me")
        (:a :class "button" :href "/blog" "Blog")
        (:a :class "button" :href "/projects" "Projects")
        (:a :class "button" :href "/skills" "My skills")))))

(defmacro style-sheet ()
  (uiop:read-file-string #P"./style.css"))

(defmacro base-html (title &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:link :rel "preconnect" :href "https://fonts.googleapis.com")
       (:link :rel "preconnect" :href "https://fonts.gstatic.com")
       (:link :href "https://fonts.googleapis.com/css2?family=Marcellus&display=swap" :rel "stylesheet")
       (:link :rel "icon" :href "/public/favicon.ico")
       (:title ,title)
       (:style (:raw (style-sheet))))
      (:body
       (header)
       (:div :class "content"
        (:h1 ,title)
        ,@body)))))

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
            (:p "Hi my name is Andreas Fuglesteg Dale. Nice to meet you!")
            (:p " Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper  iaculis est, id viverra elit ultrices a. In tempor id nisl at volutpat.  Aenean quis consequat neque. Nullam interdum consectetur odio quis  condimentum. Quisque laoreet nisl semper turpis consequat hendrerit.  Cras scelerisque suscipit pharetra. Donec efficitur dolor quis venenatis  scelerisque. Sed pulvinar maximus risus, eget vehicula mi venenatis id.
 Phasellus turpis dui, elementum vel eleifend at, convallis ut augue.  Nullam eu odio odio. Vestibulum a erat fringilla, convallis mi at,  maximus lectus. Suspendisse luctus velit id turpis venenatis sodales.  Integer lobortis quam tortor. Pellentesque ac luctus leo, tincidunt  scelerisque urna. Cras nec tempus ex. Fusce feugiat ultricies est, id  elementum quam. Nulla facilisi. Nam efficitur, odio sed convallis  vehicula, massa eros fringilla sapien, vel pharetra neque augue rutrum  libero. Vivamus iaculis ex ut cursus scelerisque. Donec iaculis aliquam  velit, ac fermentum quam ornare ac. Vivamus sit amet ex a arcu tincidunt  dignissim sit amet non justo. Vivamus a orci risus.
 Vivamus at massa est. Mauris efficitur leo eu mauris tristique  tincidunt. Suspendisse eu luctus sapien. Aenean urna sapien, gravida sed  leo malesuada, tincidunt convallis quam. Donec sagittis magna sit amet  lorem laoreet cursus. Etiam bibendum ultricies enim sed molestie. Aenean  sit amet finibus sapien, et efficitur justo. Donec facilisis est ac  nulla semper, et tempus mauris fermentum. Nullam egestas consequat ante,  sit amet dapibus magna suscipit sed. Etiam at dui pellentesque,  facilisis urna ac, ullamcorper arcu.
 Cras egestas libero non ipsum dictum, vel dignissim urna lacinia. Ut  dictum eleifend nulla at rhoncus. Nunc gravida lectus nec ante tristique  placerat. Mauris ac lectus efficitur, cursus ex eget, sodales ex.  Praesent sodales ante euismod, volutpat mauris nec, venenatis est. In  vel mattis elit, sit amet auctor enim. Suspendisse ut velit in odio  faucibus convallis. Fusce neque sapien, mattis et iaculis vel, ultricies  et purus.
 Morbi mollis mauris sit amet augue pulvinar cursus. Vivamus vitae justo  porta, molestie ante non, pellentesque eros. Nullam quis placerat metus.  Suspendisse diam purus, pellentesque non nisl eu, eleifend aliquam  ipsum. Curabitur accumsan, est vitae elementum mollis, lorem felis  elementum felis, et tristique nulla ipsum vitae orci. Phasellus aliquet,  felis et venenatis pretium, lacus nulla sollicitudin massa, non rhoncus  enim lacus eget dolor. Fusce eget velit purus. Aliquam maximus congue  velit, in aliquam sapien fringilla aliquet. Vestibulum ligula lorem,  hendrerit vitae consectetur vitae, convallis ac nunc. Nulla facilisi.")))))

(defun favicon (env)
  `(301 (:location "/public/favicon.ico")))

(defun dispatch (env)
  (let ((handler (cdr (assoc (getf env :path-info) *routes* :test #'string=))))
    (if (null handler)
        (not-found)
        (funcall handler env))))

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

(defvar *routes*
  '(("/" . index)
    ("/about" . about)
    ("/favicon.ico" . favicon)))
