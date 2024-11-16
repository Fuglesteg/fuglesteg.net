---
name: Fuglesteg.net
source-link: https://github.com/Fuglesteg/fuglesteg.net
technologies:
    - lisp
    - guix
    - docker
    - nginx
---

# Fuglesteg.net

> The website which you are looking at

This website is written in Common Lisp using mainly
[Clack](https://github.com/fukamachi/clack),
[Spinneret](https://github.com/ruricolist/spinneret),
[Parenscript](https://parenscript.common-lisp.dev/), and
[common-doc](https://commondoc.github.io/). Most pages are written in markdown
and then translated to HTML using common-doc. To add metadata to the pages I
have extended the commmondoc-markdown package to parse yaml frontmatter and add
it to the metadata of the common-doc document. This was done easily using
CLOS method qualifiers.

The rest of the application also uses a custom router that I am going to turn
into a library soon. It allows me to define routes like this:

*Oversimplified example*:

```lisp
(defroute article "/articles/{article-title}"
          (content (find article-title *articles* :key #'title :test #'string=)))
```

The program is then packaged with a Guix package definition, a GitHub action builds the package as a docker image which is then deployed to Docker Hub. The image is then deployed to my self hosted server.
