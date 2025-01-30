---
date: 14.05.2024
tags:
    - lisp
    - language design
---

# Program intent
> On the longevity and practicality of abstractions

Programming languages are special in that they attempt to formalize logical
processes, they bridge the gap between the logic of a machine and human
thinking. This article is about an area of programming I think is historically
and currently overlooked: abstraction through extensible languages. We will
explore language design, abstractions and current programming trends. I will
showcase the power of using an extensible programming language, to write clear
and maintainable code, and how it can adapt to programming paradigms. This
article is about the art of writing programs with *intent*.

In recent times there seems to be a big push within language design towards
simple languages which makes your programs processes clear. This is often done
by simplifying or removing *means of abstraction*. Languages such as Go, Nim,
Zig, Odin and Jai, have introduced us to fairly simple languages that attempt to
minimize the ways in which we can abstract logic.

I find this design choice rather limiting, I think it comes from a reaction to
some unfortunate means of abstraction in modern languages, mainly the
mainstream Object Oriented languages. (e.g. Java, C#, C++). More than it comes
from genuine analysis of the means of abstraction available in programming.
This unfortunate situation means that new mainstream languages do not focus on
what I think is the most interesting and important tool in our
programming tool belt. The tool that let's us extend and simplify our
vocabulary, **the abstraction**.

Nowadays with the hype around AI, many predict a future where programming
languages will in some way be replaced with natural language prompts passed to
an LLM which writes the program for you. While I do not wish for such a future,
I love my programming languages, I think ignoring the sentiment is a mistake. It
could occur many years in the future or might never happen in our lifetimes, the
death of programming has been predicted many times before. When I hear this
sentiment however, when I look at the growing popularity of low code platforms,
I think what is actually being said is "I wish programming was easier".

Programmers who solve hard problems rarely prioritize making the problems easier
for the next person. There is a kind of pride in having solved a difficult
problem which translates to not being understanding of people who wish it
were not so hard. These programmers do usually have valuable criticisms
though, to the solutions that attempt to ease their suffering. You might
hear that a low code solution is slow, or that it's not customisable, that
the solution might appear easy at first, but when you want to implement
something more complex it would be better and easier with traditional tools.

Is there a way then, to solve both of these problems? Can we make programming
easier without compromising on performance, power and flexibility? Is there a
way to have our cake and eat it too?

## Means of abstraction

In all modern programming languages you will have the same basic means of
abstraction: variables, conditionals, types, iteration constructs, and
subroutines. These basic means of abstraction, along with any other constructs
the language may offer, are known as the "language constructs". They define the
tools you will be using when creating abstractions in your program.

In C# or Java you will be writing all your code inside a class, the class is a
fundamental language construct you cannot be without in those languages. The
object oriented languages are designed in a way where a class is as essential as
a subroutine or a variable. These languages are truly object **oriented**. Plenty of
languages support objects and classes, but few make them an essential part of
the language that you cannot write functions which are not part of a class. This
fundamental choice led to a whole new set of software design choices that we
know as *design patterns*. Object oriented design patterns emerged from the
restriction of writing all code in classes. In a multi-paradigm language one
might simply write a function that creates an object with some special
characteristics, but in an object oriented language where do you put that
function, or rather method? The solution proposed by design patterns is to put
them in a *Factory* class. These special classes are widely used in these
languages and have become a sort of convention.

This whole hassle of describing program logic through a bunch of conventions
that we read about in some book seems... a bit arcane. These conventions are not
limited to OOP. Think of the classic **for** loop:

```c
for (int i = 0; i < 10; i++) {
    // Do something
}
```

Almost every part of this construct is something the programmer has to know
beforehand, it is not something that is intuitively understood by a basic
understanding of logic and language. It is also mostly irrelevant to what we are
trying to do. We just want some code to run 10 times, but we are having to
initialize a variable, define a condition and increment the variable. This is
also a completely esoteric construct in the language. "for" is not syntactic
sugar for a function call, or something else the user could define, "for" is a
special keyword that the compiler understands and that the programmer has to
learn. This means that the user would not be able to implement the for loop
syntax themselves using simpler constructs in the language.

You might see this is a ridiculous analysis, "of course you need to initialize a
variable and use it to iterate". The point here though is to not limit our
abstractions to the instruction set of the computer. The for loop example might
make a lot of sense for translating the statements into assembly instructions.
However, for explaining logic in a clear and concise way, it is not very
intuitive.

## Intent

What these special constructs and design patterns are then, are butchered DSLs.
A Domain Specific Language (DSL) is a language designed to specify and solve a
certain problem domain. SQL is a perfect example, it is a language that solves
the problem of interacting with databases, and it does so quite concisely and
well. Think of how easy it is to understand the basics of SQL, and how someone
with no programming experience could understand what is happening in a SQL
query.

Take for example:

```sql
SELECT name FROM users
WHERE age > 20
```

Vs. the equivalent Go code for getting a selection of items from a collection:

```go
var names []string
for _, user := range users {
    if user.age > 20 {
        names = append(names, user.Name)
    }
}
return names
```

SQL is an example of a DSL that is designed quite well, however the
language construct of a `for` loop is (in my opinion) an unfortunate mishap
of history. A simple construct that was easy to implement in the past. If you
told people to explain the concept of running code multiple times in a concise
way you would be more likely to get something like:

```
repeat 10 { }
```

The intention of the program describes the goals that the program is trying to
achieve. If the program is written in a way where the intention becomes clear
by reading the code, it is far better than reading comprehensive documentation
and code comments. Documentation and code comments can and do grow out of sync
with the code. The documentation could also be misleading if the programmer
writing it has a poor understanding of some parts of the system or if they are
unclear in the way they choose to describe the code. Documentation strays, but
code never lies.

While a DSL *can* be very good at explaining the logic of a program, it is not
always the case. In the example of Regex (Regular expressions) you have a very
terse DSL that can become extremely hard to read after only a few characters.

## Extensibility

In most programming languages we are limited by the language's constructs, we
cannot extend the language itself and so we rely on the pre shipped constructs of
any given language. In order to extend our vocabulary we either use the language
constructs in a specific precise way, creating a *convention* like the design
patterns we looked at earlier. Or you can create your own language, or make some
hacky compiler plugins for your favorite programming language.

There is however a better way, instead of creating conventions that everyone
has to precisely follow, and instead of creating our own languages and
compilers over and over we can use an *extensible programming language*. A
language were we can extend our vocabulary and grammar ourselves, not by using
some hacky compiler plugin, but by writing functions and libraries that
themselves write code. This secret weapon, known as meta programming, allows us
to make clear the intention of the program. We can write DSLs for different
tasks in the middle of the rest of our code, behind the scenes the DSL will be
translated into any other code at compile time and can run just as fast as any
equivalent low level code.

This functionality is known as a macro, a construct that allows us to define how
code should be preprocessed by the compiler. A language which supports macros in
a proper way, **not** the way that C does with it's precompiler. Allows a user
to extend the language by themselves, and for organizations and communities to
easily experiment with making DSLs and new kinds of abstractions. A language
that supports proper macros is not limited to the constructs of that language, a
user can implement any logic they wish to express as a macro.

Of the modern and popular languages, the big one that I have not yet mentioned
is Rust. Rust does support macros, and the macros run with the full power of
Rust. Rust code can be written to analyze and generate other Rust code, this is
used in some clever ways like in [SQLx](https://github.com/launchbadge/sqlx),
where the SQL queries are checked at compile time using an actual database
connection. In Rust you can make macro rules for simple transformations of code,
but you also have the ability to write a procedural macro which gives you access
to an Abstract Syntax Tree (AST) that you can manipulate to produce any code.
Learning to write procedural macros is however quite cumbersome, because they
require you to learn about the Rust AST and manipulate the tree in such a way as
to output the correct code. Rust macros can also not use any of the
functionality of the program that it is running in, so you cannot write helper
functions and such to help in writing macros. So in Rust, writing a macro
requires a different skillset than writing regular programs.

There is another language or rather family of languages that solve this problem
in a more intuitive way. The Lisp family of languages have a *homoiconic* syntax
meaning that both data and code are written in the same syntax. And this syntax
is a tree, quite close to an AST. This means that the same syntax and actions
you perform to manipulate the data in a program can be used to manipulate the
code in a program. This means that when you know Lisp, you know how to write a
macro. This makes the barrier to entry for meta programming as low as learning
the language itself.

## Longevity

Lisp is one of the oldest programming languages still in use, originally
designed by John McCarthy in 1960. Lisp lived as a bunch of dialects and
offshoots for many years until an effort started to emerge attempting to unify
the dialects, this effort became the language now known as Common Lisp. Common
Lisp got it's latest specification in 1994, but because of the extensibility of
the language the community has managed to iterate and innovate on the language.
Libraries like [Alexandria](https://alexandria.common-lisp.dev/) and
[Serapeum](https://github.com/ruricolist/serapeum) have introduced new macros
and functions making the language feel more modern. The extensibility of the
language means that the community has been able to take the best parts of other
languages and integrate them into their own. This feature of the language means
that it can, in theory, never die. Any new programming paradigms that might
emerge in the future can be implemented in Lisp using it's meta programming
capabilities.

The most famous example of this is the story of how the Common Lisp Object
System (CLOS) was added to the language without having to change the compiler.
An entire system of OOP could be attached onto the language using the same tools
available to every other user of the language. CLOS was implemented as OOP was
getting big and made some clever innovations and smart design choices in it's
implementation.

Any user of Lisp has the ability to experiment with syntax and language design.
This freedom given to the programmer is incredibly powerful and makes the Lisp
languages a sandbox for developer experimentation and innovation. It is said
that Lisp eats it's own children, this describes the way Lisp can adapt the
features of newer languages. The Lisp community can incorporate the features of
another programming language into Lisp and people looking to experiment with new
language constructs and ideas can simply load a library and use these new
constructs.

The extensibility of Lisp means that the language doesn't really need a new
specification. Think of the history of C and C++, if C had macros like Lisp, C++
could be implemented as a library to C. The C community could themselves
experiment with different constructs they wish to use to make the language
easier to use.

## Practicality

Common Lisp comes with a few built in DSLs to solve common problems. The LOOP
macro is perhaps the best example of a built in DSL that makes its intent very
clear. The LOOP macro allows you to write iteration constructs in a very clear
and concise way. Take the previous example of filtering and collecting names in
a collection, using LOOP we could write it as:

```lisp
(loop for user in users
    when (< 20 (age user))
    collect (name user))
```

This code reads very similarly to the SQL statement we looked at earlier. If we
want it to look even more similar we could construct a simple DSL on top of the
LOOP macro.

```lisp
(defmacro select (properties &key from where)
  `(loop for item in ,from
         when ,where
         collect (list ,@(loop for property in properties
                        collect `(,property item)))))
```

Then we can use it like:

```lisp
(select (name) :from users
  :where (< 20 (age item)))
```

This is not the best way to implement this macro, but perhaps the simplest.
There are many ways to improve this further, like being able to write `age`
instead of having to specify `(age item)`, but this serves as an example of how
easy it is to extend the language in simple ways like this. In only a few lines
of code we are able to implement a simple construct that expresses our intent
very clearly.

If we want to use the LOOP to simply repeat code we can write:

```lisp
(loop repeat 10 do (<LISP CODE>))
```

This code is incredibly similar to the example we looked at previously and
speaks to how clear you can make your intentions when using a vocabulary
designed to solve a specific task. Using a DSL like this makes it completely
apparent what we wish for the program to do. A code comment explaining this
would be completely redundant. Of course **why** we want to repeat this code 10
times would still need to be documented in some way, though this could also be
embedded in the code, using function names, variable names or something similar.

The code gets rewritten at compile time into low level constructs that are very
performant. The generated code can also be introspected (expanded) while
developing so that you can analyze the code that the macro generates. This is
very helpful when developing macros yourself.

We could also write another simple macro to get almost exactly the expression we
started with, since this is itself a macro it will have no effect on the runtime
performance:

```lisp
(defmacro repeat (number-of-times &body body)
    `(loop repeat ,number-of-times do ,@body))
```

Then we can use it like:

```lisp
(repeat 10 (<LISP CODE>))
```

This style of programming allows us to first define how we wish for our code to
be read and then implement the DSL that turns it into code. It's a very
declarative way of writing and reading code and makes for incredibly clear
programs where intentions are apparent at a glance.

<hr/>

### Examples

The sky is the limit when it comes to DSLs in Lisp and other Extensible
programming languages. I will however highlight some uses of Lisp macros that I
find fascinating, some are less useful than others, but demonstrate the power
of the meta programming facilities provided by Lisp.

- [Clerk](https://github.com/tsikov/clerk) uses a simple DSL for writing CRON job like scheduled tasks.

- [Vacietis](https://github.com/vsedach/Vacietis) is a C to Common Lisp compiler. Seems a little weird at first, but can
be used to interop and extend C libraries using Common Lisp.

- [Coalton](https://github.com/coalton-lang/coalton/) is a statically typed, purely functional language that is embedded
within your Common Lisp program. It is inspired by Haskell, but still uses the
s-expression based syntax of Lisp.

- [Alexa](https://github.com/quil-lang/alexa) allows you to write lexical analyzers using a DSL.

- [Parenscript](https://parenscript.common-lisp.dev/) is a Common Lisp to JavaScript transpiler, allowing you to use many
facilities of Lisp while developing an interactive web app.

- [Spinneret](https://github.com/ruricolist/spinneret) is a library which allows you to define HTML using an s-expression
syntax. You define the HTML as a tree of keywords representing tags and
attributes, then you can compile this tree, into an HTML string.
The library also interoperates with Parenscript allowing the tree to be
transpiled into a parenscript expression that generates the HTML using
`DocumentFragment`.

- [LASS](https://github.com/Shinmera/LASS) is an s-expression based DSL for writing CSS, inspired by SASS.

- [Sxql](https://github.com/fukamachi/sxql) is an s-expression based DSL for writing SQL queries.

## Conclusion

While it may not seem like it, the point of this article was not to preach Lisp.
The point is to open the door for giving the power of abstraction into the
hands of the programmer. Extensible languages like Lisp offer an alternative
to the modern, simpler, rigid languages. To highlight the advantages of using
an extensible programming language I have attempted to show examples in the
exploration of new abstractions, the freedom of using a language that can adapt
to current trends, the usefulness of DSLs for improving readability and
maintainability of programs, and the **fun** in exploring abstractions without
limitations.

I hope you walk away with a new appreciation for programming language design and
an urge to try out writing your own abstractions in an extensible programming
language, free from the limitations of language constructs.
