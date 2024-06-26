(use-modules (gnu packages lisp)
             (gnu packages lisp-xyz)
             (gnu packages)
             (gnu packages libevent)
             (gnu packages serialization)
             (guix packages)
             (guix git-download)
             (guix build-system asdf)
             (guix gexp)
             ((guix licenses)
              #:prefix license:))

(define sbcl-common-doc
  (package
   (name "common-doc")
   (version "1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/CommonDoc/common-doc")
           (commit "bcde4cfee3d34482d9830c8f9ea45454c73cf5aa")))
     (sha256 (base32 "0bzc4w37cq5mbkd15vxziks6nq58yad04mki4nwy5w6pza7z0faa"))
     (file-name (git-file-name name version))))
   (build-system asdf-build-system/sbcl)
   (arguments '(#:tests? #f
                #:phases (modify-phases %standard-phases
                          (add-after 'unpack 'delete-unused-systems
                           (lambda _
                             (for-each delete-file
                                       '("common-doc-contrib.asd"
                                         "common-doc-gnuplot.asd"
                                         "common-doc-graphviz.asd"
                                         "common-doc-include.asd"
                                         "common-doc-plantuml.asd"
                                         "common-doc-split-paragraphs.asd"
                                         "common-doc-test.asd"
                                         "common-doc-tex.asd")))))))
   (inputs (list sbcl-trivial-types
           sbcl-local-time
           sbcl-quri
           sbcl-anaphora
           sbcl-alexandria
           sbcl-closer-mop))
  (synopsis "") (description "") (license license:expat) (home-page "")))

(define sbcl-common-html
  (package
   (name "common-html")
   (version "1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/CommonDoc/common-html")
           (commit "96987bd9db21639ed55d1b7d72196f9bc58243fd")))
     (sha256 (base32 "1i11w4l95nybz5ibnaxrnrkfhch2s9wynqrg6kx6sl6y47khq1xz"))
     (file-name (git-file-name name version))))
   (build-system asdf-build-system/sbcl)
   (arguments '(#:tests? #f))
   (inputs (list sbcl-anaphora sbcl-alexandria sbcl-common-doc sbcl-plump))
  (synopsis "") (description "") (license license:expat) (home-page "")))

(define sbcl-commondoc-markdown
  (package
   (name "commondoc-markdown")
   (version "1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/40ants/commondoc-markdown")
           (commit "7abd28806bec33f291b982684d143edddb5cc32a")))
     (sha256 (base32 "12n8yx8jhz8713r63gmrymplm1mfczm7q7a343d13wl6gng1gjs1"))
     (file-name (git-file-name name version))))
   (build-system asdf-build-system/sbcl)
   (arguments '(#:tests? #f))
   (inputs (list sbcl-3bmd 
                 sbcl-common-doc 
                 sbcl-plump 
                 sbcl-common-html 
                 sbcl-ironclad
                 sbcl-cl-str))
  (synopsis "") (description "") (license license:expat) (home-page "")))

(define fuglesteg.net
  (package
   (name "fuglesteg.net")
   (version "1.1")
   (source (local-file (dirname (current-filename)) #:recursive? #t))
   (build-system asdf-build-system/sbcl)
   (outputs '("out" "bin" "image"))
   (arguments
    (list 
     #:phases 
     #~(modify-phases %standard-phases
                      (add-after 'create-asdf-configuration 'build-program
                                 (lambda* (#:key outputs #:allow-other-keys)
                                          (define (with-build-environment directory procedure)
                                            (let ((build-dependencies '("/articles" "/projects" "/about-me.md"))
                                                  (runtime-dependencies '("/public")))
                                              (mkdir-p directory)
                                              (for-each (lambda (dependency) 
                                                          (copy-recursively (string-append "." dependency) 
                                                                            (string-append directory dependency)))
                                                        (append build-dependencies runtime-dependencies))
                                              (procedure directory)
                                              (for-each (lambda (dependency)
                                                          (delete-file-recursively (string-append directory dependency)))
                                                        build-dependencies)))
                                          (with-build-environment (string-append (assoc-ref outputs "bin") "/bin") 
                                            (lambda (directory) 
                                              (build-program (string-append directory "/fuglesteg.net")
                                                             outputs
                                                             #:compress? #t
                                                             #:entry-program '((fuglesteg.net:start)))))
                                          (with-build-environment (string-append (assoc-ref outputs "image") "/lib")
                                            (lambda (directory)
                                              (build-image (string-append directory "/fuglesteg.net")
                                                           outputs))))))))
   (inputs (list sbcl
                 sbcl-serapeum
                 sbcl-alexandria
                 sbcl-cl-yaml
                 sbcl-spinneret
                 sbcl-parenscript
                 sbcl-common-doc
                 sbcl-commondoc-markdown
                 sbcl-common-html
                 sbcl-clack
                 sbcl-lack
                 sbcl-woo))
   (synopsis "") (description "") (license license:expat) (home-page "")))
