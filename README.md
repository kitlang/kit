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

*Kit is pre-alpha and not all features are fully implemented; see the [roadmap on Trello](https://trello.com/b/Bn9H0fzk/kit).*

License
-------

The Kit compiler is licensed under the GNU Lesser General Public License; see the accompanying [LICENSE.md](https://github.com/kitlang/kit/blob/master/LICENSE.md) file. This applies to modifications to the compiler source itself; any code you write and compile with Kit is yours to license however you choose.

The Kit standard library (.kit files contained in this repo) is released under the [MIT license](https://github.com/kitlang/kit/blob/master/LICENSE-RUNTIME.md).

Design goals and philosophy
---------------------------

- Magic and abstracting away complexity are good! Developers should write the most concise and declarative version of their code, and use syntax transformations to convert it into what a performance-conscious developer would've written by hand.

- Expose the necessary low-level details to write high performance code, and make it easy to abstract them away without entirely giving up control.

- Kit provides more compile-time safety than C, but never chooses safety at the expense of ergonomics.


Building from source
--------------------

The Kit compiler, `kitc`, is written in Haskell. Building the compiler requires GHC; the easiest path is to install [Stack](https://docs.haskellstack.org/en/stable/README/) and run:

    stack build

This will install all other dependencies locally, including a local GHC binary, and build the compiler.

To run the compiler unit tests:

    stack test

To install:

    stack install

This will copy the `kitc` binary to Stack's binary install directory (~/.local/bin on Linux); make sure this directory is part of your executable paths.

You'll need to point Kit to its standard library (which is the "std" directory in the repo's root directory); you have a few options:

- Set an environment variable, KIT_STD_PATH
- Put the kitc binary next to its standard library
- Put the standard library in an OS-specific default location:
    - Linux: "/usr/lib/kit"
    - Mac: "/usr/local/lib/kit"

### Windows setup

- Kit is currently known to work with mingw64; anything else is unsupported at this time.

Install from package
--------------------

### Debian/Ubuntu

* Setup Bintray's APT key:

(Debian might need to install dirmngr first - `sudo apt install dirmngr`, and the HTTPS transport for apt - `sudo apt install apt-transport-https`)

```sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 379CE192D401AB61 ```

* Using the command line, add the following to your /etc/apt/sources.list system config file:

For prereleases:

```echo "deb https://dl.bintray.com/kitlang/kitlang-prerelease-ubuntu bionic universe" | sudo tee -a /etc/apt/sources.list.d/kitlang-prerelease.list```

For stable releases:

```echo "deb https://dl.bintray.com/kitlang/kitlang-stable-ubuntu bionic universe" | sudo tee -a /etc/apt/sources.list.d/kitlang-stable.list```

Or, add the repository URLs using the "Software Sources" admin UI:

```deb https://dl.bintray.com/kitlang/kitlang-prerelease bionic universe```

* Update apt and install:

```sudo apt update```

```sudo apt install kitlang```

### RedHat/CentOS

* Run the following to get a generated .repo file:

For prereleases:

```wget https://bintray.com/kitlang/kitlang-prerelease-redhat/rpm -O bintray-kitlang-prerelease-redhat.repo```

For stable releases:

```wget https://bintray.com/kitlang/kitlang-stable-redhat/rpm -O bintray-kitlang-stable-redhat.repo```

\- or -

* Copy this text into a 'bintray-kitlang-prerelease-redhat.repo' file on your Linux machine:

```
#bintraybintray-kitplummer-kitlang-prerelease-redhat - packages by kitplummer from Bintray
[bintraybintray-kitplummer-kitlang-prerelease-redhat]
name=bintray-kitplummer-kitlang-prerelease-redhat
baseurl=https://dl.bintray.com/kitlang/kitlang-prerelease-redhat
gpgcheck=0
repo_gpgcheck=0
enabled=1
```

* Then, move the config

```sudo mv bintray-kitlang-rpm.repo /etc/yum.repos.d/```

* Update yum with:

```sudo yum update```

* Install with yum:

```sudo yum install kitlang```

### macOS

* Download one of the macOS packages from https://bintray.com/kitlang/kitlang-macos/kitlang/0.1.0#

* Install the package (NOTE: the installer will complain about 'unidentified developer', have to bypass in System Preferences/Security & Privacy.)

Hello world
-----------

After building/installing `kitc`:

- Create a a new file, "helloworld.kit", and copy the following into it:

```kit
function main() {
    printf("%s\n", "Hello from Kit!");
}
```

- Run `kitc helloworld.kit --run` to compile and run your program

Copyright
---------

Copyright (C) 2018 Ben Morris. (See the [LICENSE](https://github.com/kitlang/kit/blob/master/LICENSE.md).)
