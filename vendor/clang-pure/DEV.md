# Development guide <small>[&#8617;][readme]</small>

## Install prerequisites

You'll need a recent version of [LLVM/Clang][llvm]. clang-pure has been tested
with LLVM version [3.8.1][llvm381].

### Linux

Install the LLVM version [3.8.1][llvm381] package or install from your distro's
official repositories, if the package version is recent enough. For example, on
Ubuntu:

```bash
$ sudo apt-get install libclang-dev
```

Note that the pre-built versions of LLVM packaged for Centos are typically too
old and so you'll need to install LLVM version [3.8.1][llvm381] or build it
from source.

### Windows

Install the LLVM version [3.8.1][llvm381] package and install to the default
location, which is typically `%PROGRAMFILES%\LLVM`.

### Mac OS X

Build LLVM version [3.8.1][llvm381] from source or install using
[Homebrew][brew] (easiest):

```bash
$ brew tap homebrew/versions
$ brew install llvm38
```

## Building

Build using [Stack][stack] from the repository root:

```bash
$ stack setup
$ stack build
```

If the setup script is unable to detect your LLVM/libclang library paths
automatically, you can override the default search location using the
`CLANG_PURE_LLVM_INCLUDE_DIR` and `CLANG_PURE_LLVM_LIB_DIR` variables and then re-run the build.

On Linux or Mac OS X:

```bash
$ CLANG_PURE_LLVM_INCLUDE_DIR=/path/to/llvm/include CLANG_PURE_LLVM_LIB_DIR=/path/to/llvm/lib stack build
```

On Windows:

```cmd
> set CLANG_PURE_LLVM_INCLUDE_DIR=C:\Path\To\LLVM\include
> set CLANG_PURE_LLVM_LIB_DIR=C:\Path\To\LLVM\bin
> stack build
```

The default search location for libraries and include directories is `/usr`
under Linux and Mac OS X and `%PROGRAMFILES%\LLVM` on Windows.

[brew]: http://brew.sh/
[llvm]: http://llvm.org/
[llvm381]: http://llvm.org/releases/download.html#3.8.1
[readme]: README.md
[stack]: https://haskellstack.org/
