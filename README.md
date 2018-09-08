[![Build Status](https://img.shields.io/travis/kitlang/kit/master.svg?style=flat)](https://travis-ci.org/kitlang/kit)
[![License: LGPL v3](https://img.shields.io/badge/license-LGPL%20v3-202020.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Join the chat at https://gitter.im/kitlang/kit](https://img.shields.io/badge/chat-on%20gitter-f50864.svg)](https://gitter.im/kitlang/kit?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![roadmap on_trello](https://img.shields.io/badge/roadmap-on%20trello-0079bf.svg)](https://trello.com/b/Bn9H0fzk/kit)
[![Website URL: https://www.kitlang.org](https://img.shields.io/badge/web-kitlang.org-3fa5bf.svg)](https://www.kitlang.org)

![Kit logo](https://raw.githubusercontent.com/kitlang/kit/master/assets/logo-128.png)

```ascii
                       ,  ,
  _,-=._              /|_/|
  `-.}   `=._,.-=-._.,  @ @._,
     `._ _,-.   )      _,.-'
        `    G.Q-^^u`u'

  oooo    oooo   o8o      ,
  `888   .8P'    `V'    ,o8
   888  d8'     oooo  .o888oo
   88888[       `888    888
   888`88b.      888    888
   888  `88b.    888    888 ,
  o888o  o888o  o888o   '888'
```

[**Kit**](https://www.kitlang.org) is a programming language designed for creating concise, high performance cross-platform applications. Kit compiles to C, so it's highly portable; it can be used in addition to or as an alternative to C, and was designed with game development in mind.

Why you should use Kit in place of:

| C/C++ | a higher level language |
| --- | --- |
| Modern language features: type inference, algebraic data types, pattern matching, explicit function inlining, automatic pointer dereferencing, generics, implicits. | Full control over performance: pointers, manual memory management, no GC (unless you introduce it yourself, which is easy!) |
| A more expressive type system, including traits for polymorphism, and abstract types, which provide custom compile-time behavioral and type checking semantics to existing types with no runtime cost. | Metaprogramming via a typed term rewriting system; use rules to transform arbitrary expressions at compile time based on their type information. Create your own interface or DSL. |
| A sane, easy to use build system. Kit features modules, imports, and standard package structure, plus a simple but powerful build tool: manage your project via a simple YAML configuration file and `kit build`, `kit test`, or `kit run`. (coming soon...) | Take advantage of existing C libraries without any wrappers; just include the header and directly use types/functions/variables. |

```kit
function main() {
    var s: CString = "Hello from Kit!";
    printf("%s\n", s);
}
```

**[See more code examples here](https://www.kitlang.org/examples.html)**

Kit is licensed under the GNU Lesser General Public License; see the accompanying [LICENSE.md](https://github.com/kitlang/kit/blob/master/LICENSE.md) file. This applies to modifications to the compiler source itself; any code you write and compile with Kit is yours to license however you choose.

*Kit is pre-alpha and not all features are fully implemented; see the [roadmap on Trello](https://trello.com/b/Bn9H0fzk/kit).*


Design goals and philosophy
---------------------------

- Magic and abstracting away complexity are good! Developers should write the most concise and declarative version of their code, and use syntax transformations to convert it into what a performance-conscious developer would've written by hand.

- Expose the necessary low-level details to write high performance code, and make it easy to abstract them away without entirely giving up control.

- Kit provides more compile-time safety than C, but never chooses safety at the expense of ergonomics. Kit gives you plenty of rope to hang yourself with. Use it to climb a mountain, not hang yourself! (If you do hang yourself, a detailed error message will let you know.)


Building from source
--------------------

The Kit compiler, `kitc`, is written in Haskell. Building the compiler requires GHC; the easiest path is to install [Stack](https://docs.haskellstack.org/en/stable/README/) and run:

    stack build

This will install all other dependencies locally, including a local GHC binary, and build the compiler.

To run the compiler unit tests:

    stack test

To point Kit to its standard library, you have a few options:

- Set an environment variable, KIT_STD_PATH
- Put the kitc binary next to its standard library
- Put the standard library in an OS-specific default location:
    - Linux: "/usr/lib/kit"
    - Mac: "/usr/local/lib/kit"


Hello world
-----------

After building/installing `kitc`:

- Create a "src" directory, and within it, a new file, "helloworld.kit"
- Copy the following into src/main.kit:

```kit
include "stdio.h";

function main() {
    printf("%s\n", "Hello from Kit!");
}
```

- Run `kitc helloworld --run` to compile and run your program

Copyright
---------

Copyright (C) 2018 Ben Morris. (See the [LICENSE](https://github.com/kitlang/kit/blob/master/LICENSE.md).)
