# cmacro: Lisp macros for C

# Usage

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

# Examples

## Lambdas

## Anaphoric if

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
the AST to an XSLT program, but this seemed like the best way.

The Makefile is largely based on that of
[Dimitri Fontaine](http://tapoueh.org/)'s
[pgloader](https://github.com/dimitri/pgloader) utility.

Peter Norvig's **Paradigms of Artificial Intelligence Programming** chapter on
Eliza was used as a reference for the pattern-matching engine.

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
