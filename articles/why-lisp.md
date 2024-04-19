---
tags:
    - lisp
date: 17.04.2024
---

# Why lisp

My journey to lisp started when I got frustrated with Go. I spent some time
getting into and using Go, for about a year I wrote a few different web server
projects in Go (Mainly REST APIs). But it was only when I was working on
[Timid](/articles/timid) that I realized that this language was probably not
for me.

Go is an amazing language for getting stuff done, but I was getting annoyed by
it's rigid design. That is part of the reason that makes Go amazing for getting
stuff done quickly, the fact that you don't get distracted by language features
and choosing the best semantics for any given task means that features get
shipped quickly in Go.

I was however wondering if there was a better way of doing things. Exploring
abstractions in Go is hard, and organizing code into useful abstractions can be
quite tricky. The design of the language and the sentiment of the Go community
seems to be that simplicity of the language is what makes Go... Go. The design
favors clarity of the programs processes instead of the clarity of the programs
intent. That is to say, Go favors laying in front of the programmer, the exact
processes that the program executes. This is a fundamental design choice of
many modern languages (e.g. zig, odin, jai, nim) and signifies a trend in
language design.

The problem is that I find this design decision a bit boring. Not to
say that I don't see the value, but I am more interested in how we can use
programming language as languages. Using a programming language to
communicate what the intent of a program is.

I don't mean that I wish to write my programs in natural language, that is a
mistake waiting to happen. But I think exploring the ways we can abstract and
explain logic is much more fruitful than any other language design decision. In
my eyes the point of a programming language is to communicate a logical process
in clear terms to the reader. Because of this I find it hard to justify the use
of languages that clearly restrict abstractions. This is a fundamental
difference in how languages are built. I don't want a language to be restricted
to viewing logic through the instruction set of a computer. A language should
allow the programmer to explain logic freely. A good program is one
that clearly explains both the problem and the solution in clear terms.

This idea brought me to Lisp, or rather the "lisp family of languages". It's
hard to boil down what exactly defines a Lisp language (or dialect). Modern
languages like Julia makes it harder to define nowadays. I think the most
important features are it's meta programming capabilities and the huge focus on
interactivity and introspection. It was in fact this meta programming that I
found so interesting when I decided to learn some Lisp.

I did however have to pick a specific lisp dialect to learn. I considered
Clojure for a while, but was convinced somewhere along the line that Common lisp
was the way to go. Common lisp is kind of the grand dad of the lisp world. It
has a direct lineage from the original lisp designed by McCarthy and the
different dialects that emerged during that time. It is a kind of kitchen sink
of languages, it is multi paradigm and supports basically every feature of a
modern language and what defines a lisp. What it doesn't natively support is
easily added by the lisp community using it's meta programming capabilities.
This makes the language incredibly resilient in the modern world of ever
changing paradigms and language trends. If a new paradigm or feature becomes
popular it can be added to the language after the fact. The most classic example
of this is possibly the implementation of CLOS the Common Lisp Object System.
Which was introduced to the language without having to make any changes to the
compiler.

Common lisp got it's latest specification in 1994, but the community has been
able to implement many modern features into the language. I like using
[Alexandria](https://alexandria.common-lisp.dev/) and
[Serapeum](https://github.com/ruricolist/serapeum) as modern language
extensions. There are also libraries for regex, pattern matching and many others
that can be used to extend the language with features that feel native. In most
languages the users have to wait for the maintainers to make new specifications
or make changes to the compiler. In Common Lisp the users have the power to
experiment with and make any changes they want to the language.

It is said that lisp eats it's own children. This refers
to the fact that when any new lisp or lisp inspired language introduces new features. The lispers in other communities will usually take whichever ideas they like and implement them into whatever lisp they use. This is the case for the threading and magic wand macros for clojure


- Possibly mention JDH compiler extension and macro hell.
- Shirakumo and Trial
- Projects like Nyxt, Lem, stumpwm (learned from watching Drew)
- Stop writing dead programs
- Code that solves a problem should be condensed to the code that actually solves the problem.
    An abstraction is good if it helps the code to focus on the problem it is trying to solve
    
- Using go I felt that in order to create good abstractions I was slowing down the program.
- Compare with Svelte and Solid
    - Instead of building a compiler you can leverage the meta programming of the language
