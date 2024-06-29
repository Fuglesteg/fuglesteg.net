# fuglesteg.net

A webpage written in Common Lisp and Markdown documents.

It uses common-doc to tranlate Markdown documents to HTML and cl-yaml to read
YAML frontmatter from the documents. (See: `document.lisp`)

The web server is built on top of Clack, but it only serves static content so
the plan is to make it generate static HTML files in the future. Routing is
handled using a custom DSL `defroute` in the future I will expand this and make
it a separate library.

The site is packaged using Guix, the docker container is built from this
package definition using `guix pack`. (See
`.github/workflows/build-docker-image.yml`)
