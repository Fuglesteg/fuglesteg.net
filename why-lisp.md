---
tags:
    - lisp
date: 17.04.2024
---

# Why lisp

My journey to lisp started with my exploration of different programming
languages. I spent some time getting into and using Go, for about a year I
experimented with writing a few different web server projects in Go (Mainly
REST APIs). It was when I was working [Timid](/articles/timid) that I realized
that this language was probably not for me.

Go is an amazing language for getting stuff done, but I was getting annoyed by
it's rigid design. That is what makes Go amazing for getting stuff done
quickly, the fact that you don't get distracted by language features and
choosing the best semantics for any given task means that features get shipped
quickly in Go.

I however was wondering if there was a better way of doing things. Exploring
abstractions in Go is hard, and organizing code into useful abstractions can be
quite tricky. It seems to be the sentiment in the Go community that simplicity
of the language is what makes Go... Go. The design seems to favor clarity of
the programs processes not the clarity of the programs intent. That is to say,
Go favors laying in front of the programmer, the exact processes that the
program executes. This is a fundamental design choice of the language and a
clear direction that is growing within the programming community (examples...)

The problem is that I fundamentally disagree with this design decision, not to
say that I don't see the value, but I am more interested in how we can use
programming language as languages. Using a programming language to
communicate what the intent of a program is.

Not to say that I wish to communicate my programs in natural language, that is
a mistake waiting to happen.

For the last year or so I have been delving deep into the Lisp family of languages.

- Stop writing dead programs
- Code that solves a problem should be condensed to the code that actually solves the problem.
    An abstraction is good if it helps the code to focus on the problem it is trying to solve
    
- Using go I felt that in order to create good abstractions I was slowing down the program.
- Compare with Svelte and Solid
    - Instead of building a compiler you can leverage the meta programming of the language
