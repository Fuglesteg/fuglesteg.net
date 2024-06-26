(defpackage fuglesteg.net
  (:use :cl 
        :spinneret 
        :alexandria 
        :serapeum 
        :fuglesteg.net/document
        :fuglesteg.net/routing)
  (:export :start))

(in-package #:fuglesteg.net)

;;; Application

(defvar *app*
  (lack:builder
   (:static :path "/public/"
    :root #P"./public")
   (lambda (env) (handle-route env))))

(defvar *handler* nil)

(defun start-debug ()
  (when *handler*
    (clack:stop *handler*))
  (setf *handler* (clack:clackup *app*)))

(defun start ()
  (setf *handler* (clack:clackup *app* 
                                 :server :woo
                                 :address "0.0.0.0"
                                 :port 80
                                 :use-thread nil
                                 :debug nil)))

(defun handle-route (env)
  (handler-case (dispatch env)
    (http-not-found () (not-found))))

;;; Markup

; Add closing tags
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

(defun not-found ()
  `(404 (:content-type "text/html")
        (,(base-html "404 - Not Found"
            (:p "The page could not be found ;(")))))

(deftag contact (body attrs)
  `(progn
     (:h2 "Find me on:")
     (:a :href "https://linkedin.com/in/andreas-fuglesteg-dale" (:img :style "width: 4rem" :src "/public/icons/linkedin.svg"))
     (:a :href "https://github.com/Fuglesteg" (:img :style "width: 4rem" :src "/public/icons/github.svg"))))

(defvar *about-document* (make-instance 'document :file-path #P"./about-me.md"))

(defmacro image-style-sheet ()
  (uiop:read-file-string #P"./image.css"))

(defpage about "/about" "About me"
  (:style (:raw (image-style-sheet)))
  ;(:img :class "rounded profile-picture" :src "/public/profile-picture-zoomed.jpg")
  (:img :class "rounded profile-picture" :src "/public/profile-pic-2.jpg")
  ;(:img :class "rounded profile-picture" :src "/public/profile-picture.jpeg")
  (:raw (content *about-document*)))

;;; Articles

(defvar *articles-list* (make-instance 'document-list :type 'article :directory #P"./articles"))

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

(deftag enable-highlights (body attrs)
  `(progn
     (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/tomorrow-night-bright.min.css")
     (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js")
     (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/go.min.js")
     (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/lisp.min.js")
     (:script (:raw (ps:ps (ps:chain hljs (highlight-all)))))))

(defpage articles "/articles" "Articles"
  (articles-list))

(defpage article-page "/articles/{title}" nil
  (arrow :link "/articles" :content "Articles")
  (enable-highlights)
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
        (signal 'http-not-found))))

(defvar *devicon-overrides*
  '("lisp" "guix" "github"))

(deftag devicon (body attrs &key name)
  `(let ((src (if (member ,name *devicon-overrides* :test #'string=)
                  (format nil "/public/icons/~a.svg" ,name)
                  (format nil "https://cdn.jsdelivr.net/gh/devicons/devicon@latest/icons/~a/~:*~a-original.svg" ,name))))
     (:img 
      :title ,name
      :alt (format nil "~a icon" ,name)
      :style "width: 10%;"
      :src src)))

(deftag project-thumbnail (body attrs &key project)
  `(:a :href (format nil "/projects/~a" (name ,project))
    (:div :class "thumbnail"
     (:h2 (name ,project))
     (:p (synopsis ,project))
     (:div (loop for technology in (technologies ,project)
                 collect (devicon :name technology))))))

;; Projects

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
  (ps:with-ps-gensyms (gallery-container scrolling)
    `(progn
       (:style (:raw ,(repeating-gallery-style-sheet)))
       (:script :type "module"
        (:raw "var __PS_MV_REG;") ; Annoying bug
        (:raw (ps:ps
                (defparameter ,scrolling nil)
                (set-interval (lambda () (when (not ,scrolling) (ps:chain ,gallery-container (scroll-by (ps:create left 3 behavior "smooth"))) 1)))
                (ps:chain ,gallery-container
                          (add-event-listener "wheel" 
                                              (lambda (e)
                                                (setf ,scrolling t)
                                                (set-timeout (lambda () (setf ,scrolling nil)) 500)
                                                (ps:chain e (prevent-default))
                                                (let ((scrolling-right (> (ps:@ ,gallery-container scroll-left) 
                                                                          (- (- (ps:@ ,gallery-container scroll-width) 
                                                                                      (ps:@ ,gallery-container client-width))
                                                                                   50)))
                                                      (scrolling-left (< (ps:@ ,gallery-container scroll-left) 50)))
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
                                                (ps:chain ,gallery-container (scroll-by (ps:create
                                                                                        left (* 5 (ps:@ e delta-y))
                                                                                        behavior "smooth")))))))))
       (:div :id ,(ps:symbol-to-js-string gallery-container)
        :class "gallery-container" 
        (loop for image in ,images
              do (:img :class "gallery-item" :src image))))))

(defpage project "/projects/{name}" nil
  (:style (:raw (articles-style-sheet)))
  (enable-highlights)
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
          (:div :style "width: 50%;"
           (loop for technology in (technologies project)
                do (devicon :name technology)))
          (:raw (content project))
          (repeating-gallery :images (images project)))
        (signal 'http-not-found))))

(defpage index "/" "Fuglesteg.net"
  (:p "Hello! Welcome to my corner of the internet." 
   (:br) 
   "Here I write about my coding projects and other things that I find interesting."
   (:br)
   (:a :class "link" :href "/projects/fuglesteg.net" "Read about how this page was made"))
  (contact)
  (:hr)
  (:h2 "Recent articles")
  (articles-list))

