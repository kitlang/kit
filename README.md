![Kit logo](https://raw.githubusercontent.com/kitlang/kit/master/assets/logo-512.png)

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

**Kit** is a programming language designed for creating high performance cross-platform applications. Kit compiles to C, so it's highly portable; it can be used in addition to or as an alternative to C, over which it provides several advantages (see [examples](https://github.com/kitlang/kit/blob/master/EXAMPLES.md)):

* Modern language features such as type inference, algebraic data types, pattern matching, explicit function inlining, automatic pointer dereferencing, and generics.

* A powerful type system, including polymorphism via traits and abstract types, which provide custom compile-time behavioral and type checking semantics to existing types with no runtime cost.

* Seamless interoperability with existing C libraries. Kit compiles to C and its type system exposes C types directly. You can call C functions from Kit or Kit functions from C, by directly including header files; no bindings or wrappers are required.

* A simple but powerful build tool, written in Kit: manage your project via a simple YAML configuration file and `kit build`, `kit test`, or `kit run`. (coming soon...)

Kit is licensed under the GNU Lesser General Public License; see the accompanying LICENSE.md file. This applies to modifications to the compiler source itself; any code you write and compile with Kit is yours to license however you choose.

Kit is pre-alpha and not all features are fully implemented; see the [roadmap on Trello](https://trello.com/b/Bn9H0fzk/kit).


Design goals and philosophy
---------------------------

- Kit prefers "magic" and expressiveness over boilerplate and code that maps 1:1 to its runtime semantics. Using syntax transformation, developers should write the most concise and declarative version of their code, and it should compile to what a performance-conscious developer would've written by hand.

- Expose enough low-level details to write high performance code, but their usage should be wrapped by higher-level abstractions. Memory management in Kit is manual, but optional features like smart pointers exist (and can be defined in user space) to make it less painful.

- Verify that the program is safe at compile time, but balance safety with ergonomics.


Building from source
--------------------

The Kit compiler is written in Haskell, and building the compiler requires GHC; the easiest path is to install [Stack](https://docs.haskellstack.org/en/stable/README/) and run:

    stack build

This will install all other dependencies locally, including a local GHC binary, and build the compiler.

To run the compiler unit tests:

    stack test
