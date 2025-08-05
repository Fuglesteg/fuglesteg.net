---
name: Language interoperability library
technologies:
  - lisp
  - guix
date: 05.10.2025
---

# Language interoperability library

> Writing bilingual programs

cl-guile is a library that acts as an interoperability layer between Guile Scheme and Common Lisp. It allows you to write and call scheme functions from lisp or call lisp functions from scheme. This allows you to write sophisticated programs in both languages. You can choose to write smaller or bigger parts of the program in either language. Using the library will also allow you to use libraries from guile and lisp in the same program.

The library can also be used to embed a scheme scripting environment in a Common Lisp application.

The inspiration for the library came from using GNU Guix to setup common lisp environments. I basically wanted a guix based alternative to quicklisp. While I haven't made that alternative yet, I will most certainly be using this library when I inplement it in the future. Another use case is using it to integrate programs like Lem and my window manager StumpWM with my guix environments.

Example of defining a lisp function callable from scheme and then calling it from scheme:

```lisp
(define-scheme-procedure say-hello (name)
  (format nil "Hello ~a" name))

(scheme (say-hello "Bob")) => "Hello Bob"
```

I learnt a lot from this project. I got to try out using the FFI capabilities of Common Lisp, API design, and the inner workings of Guile.

The library currently supports the following features:

- Write Scheme expressions as part of your Lisp code
- Define Scheme code scheduled to be evaluated when library initializes
- Define Lisp functions callable from Scheme
- Define Scheme functions callable from Lisp
- Define record to CLOS object conversion
- Use define* style syntax
- Mix Scheme and Lisp expressions
- Readsyntax for `#t` and `#f
