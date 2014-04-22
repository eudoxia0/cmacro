# cmacro: Lisp macros for C

# Usage

Macros are written directly in the source, and the `cmc` program is used to
process a file with macros to a macroexpanded file.

```bash
cmc code.cm -o code.c
```

# Installing and Using

1. Install SBCL through your favorite package manager:

```bash
apt-get install sbcl
pacman -S sbcl
yum install sbcl
brew install sbcl
```

2. `make`
3. `sudo make install`

# What?

A macro is a function that operates on your code's abstract syntax tree rather
than values. Macros in cmacro have nothing to do with the C preprocessor except
they happen at compile time, and have no knowledge of run-time values.

In cmacro, a macro maps patterns in the code to templates. A macro may have
multiple cases, each matching multiple patterns, but each producing code through
the same template.

Macros are not primarily about safety and performance[1]: They are about the
programmer. Macros give you automation, plain and simple. They allow you to
abstract away and remove repetition in places where a functional or
object-oriented approach can't. For example, Common Lisp's
[WITH-OPEN-FILE](http://clhs.lisp.se/Body/m_w_open.htm) macro helps with the
common pattern of 'acquire a resource, apply something to it, and close
it'. While this can be done in languages that support (And have simple syntax
for) anonymous functions, macros help reduce this syntactico overhead.

cmacro has a very lenient notion of C syntax, which means you can write macros
to implement DSLs with any syntax you like. You could implement Lisp-like prefix
notation, or a DSL for routing URLs, or the decorator pattern, for example.

For a very simple example, this very simple macro matches anything of the form
`unless <cond>`, where `<cond>` is any arbitrary expression, and performs a
simple transformation:

```c
macro unless {
  case {
    match {
      $(cond)
    }
    template
      (!$(cond))
    }
  }
}
```

With this definition, code like `unless(buffer.empty)` becomes `if(!(buffer.empty))`.

A more complicated macro can match multiple patterns, like the `route` macro
which implements a DSL for defining routes in a hypothetical C web framework.

```c
macro route {
  /* Route all requests to 'url' to 'route'. Optionally discriminate by HTTP
  method (GET by default). */
  case {
    match {
      $(url) => $(route)
    }
    template {
      register_route($(url), $(route), HTTP_GET);
    }
  }
  case {
    match {
      $(url) [$(method)] => $(route)
    }
    template {
      register_route($(url), $(route), $(method));
    }
  }
}
```

# Why?

Because a language without macros is a tool: You write application with it. A
language with macros is building material: You shape it and grow it *into* your
application.

There is a sweet spot between low-level performance and control and high-level
metaprogramming that is not yet occupied by any language: Metaprogramming, being
an inherently compile-time thing, can be done in the absence of automatic memory
management or dynamic typing. [Rust](http://www.rust-lang.org/) seems to want to
fill this, and I also approached this problem with
[Hylas Lisp](https://github.com/eudoxia0/Hylas-Lisp), but I feel this approach
of adding metaprogramming to C - A simple language, with a long history, that
runs truly everywhere - can become useful.

# Examples

## `lambda`

```c
macro lambda {
  case {
    match {
      $(args) -> $(ret) $(body)
    }
    toplevel {
      $(ret) $(@gensym lambda) $(args) $(body)
    }
    template {
      $(@getsym lambda 0)
    }
  }
}
```

## Anaphoric `if`

This stores the result of the condition in the variable `it`. See
[Anaphora](http://common-lisp.net/project/anaphora/) for a collection of similar
anaphoric macros.

```c
macro aif {
  case {
    match {
      $(cond)
    }
    template {
      typeof($(cond)) it = $(cond);
      if(it)
    }
  }
}
```

## `forEach`

```c
macro forEach {
  case {
    match {
      ($(item), $(collection)) $(body)
    }
    template {
      {
        size_t index;
        typeof($(collection)[0]) $(item);
        for(index = 0, item = nth($(collection), 0);
            index < length($(collection));
            index++)
        $(body)
      }
    }
  }
}
```

# Variables

The syntax for variables is just a name followed by an optional,
space-separatedl list of *qualifiers*, enclosed in the `$()` operator, eg:
`$(var)`, `$(body ident)`, `$(arg const)`.

## Qualifiers

- None: The variable matches any expression.
- `rest`: Match multiple expressions (Like C's `...`).
- `ident`: Matches Identifiers.
- `int`: Integers.
- `float`: Floats.
- `num`: Integers and floats.
- `string`: String literals.
- `const`: The equivalent of `(or int float string)`.
- `op`: Operators.
- `list`, `array`, `block`: Matches expressions of the form `(...)`, `[...]`,
  `{...}`.

# Template operations

These use regular variable syntax but the text starts with a '@'.

- `gensym <label>`: Generates a unique identifier associated with `label`.
- `getsym <label> [n]`: Gets the latest identifier associated with `label`, or
  optionally the `n`-th last identifier.
- `to-string <var>`: Since string literals in C can contain variable notation,
  you have to explicity use this to stringify a variable. Note, also, that C
  concatenates string that appear next to each other in the source.
- `splice <var>`: If `var` is a block (ie `(...)`, `[...]`, `{...}`) this
  expression removes the block separators, leaving just the block body.

# Acknowledgments

The [lex](http://www.quut.com/c/ANSI-C-grammar-l-2011.html) and
[yacc](http://www.quut.com/c/ANSI-C-grammar-y.html) grammars were originally
posted by Jeff Lee in 1985, and rescued and updated to the recent standards by
[Jutta Degener](mailto:jutta@pobox.com).

The syntax for macro definition was inspired by Mozilla's great
[sweet.js](http://sweetjs.org/) library. Originally I considered multiple
different ways of defining them, including external YAML files or just piping
the AST to an XSLT or similar program, but this seemed like the best way.

The Makefile is largely based on that of
[Dimitri Fontaine](http://tapoueh.org/)'s
[pgloader](https://github.com/dimitri/pgloader) utility.

Peter Norvig's **Paradigms of Artificial Intelligence Programming** chapter on
Eliza was used as a reference for the pattern-matching engine.

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
