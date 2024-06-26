(asdf:defsystem #:fuglesteg.net
  :author "Andreas Fuglesteg Dale <andreasfdale@gmail.com>"
  :maintainer "Andreas Fuglesteg Dale <andreasfdale@gmail.com>"
  :license "GPL3"
  :version "1.0"
  :serial t
  :depends-on (#:cl-yaml
               #:spinneret
               #:spinneret/ps 
               #:serapeum 
               #:woo
               #:alexandria 
               #:common-doc 
               #:commondoc-markdown 
               #:common-html
               #:clack 
               #:lack)
  :components ((:file "document")
               (:file "routing")
               (:file "fuglesteg.net" :depends-on ("routing" "document"))))
