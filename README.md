**Potential users beware!** This is basically a glorified personal
shell script (and in fact is literally a smarter Haskell
reimplementation of what I used to do using shell scripts.) It's
designed to do what I want, and very likely doesn't do what you want
(or does it poorly or buggily.) You might want to look at
[hi](https://github.com/fujimura/hi), a very similar and much more
full-featured tool!

The *charter* tool is a tool for setting up Haskell projects. It has
three basic modes of operation which correspond to three kinds of
projects:

- `charter quick foo` creates a project called `foo` that contains a
  single executable whose entry point is in `src/Main.hs`. This is
  good for quick-and-dirty Haskell executables that need little extra
  scaffolding.
- `charter library foo` creates a project called `foo` that
  contains a library whose sources are in `src`.
- `charter executable foo` creates a project called `foo` that
  contains a binary whose entry point is in `foo/Main.hs` as well as a
  library whose sources are in `src`, and also adds a built-in `foo`
  dependency to the executable.

Some of the information needed to set up these projects is grabbed
from command-line tools, and in particular from `git` (the author and
maintainer name and email) and from `date` (the current year for
copyright information). Other pieces of information are commented out,
but can also be provided via command-line flags (such as the category,
synopsis, description, and license). Command-line flags can also add
extra binary targets, add modules to the library, or add dependencies.

Note that the flags are processed in-order and sometimes order will
affect the output.

# Example Usage

Create a library `foo` that exposes two modules, `Data.Foo` and
`Data.Bar`, and has a dependency on `bytestring`.

```bash
$ charter library foo \
    -m Data.Foo \
    -m Data.Foo.Bar \
    -a bytestring
```

Create a simple executable `cat` that depends on `text`, and has a
filled-in category and synopsis

```bash
$ charter quick haskcat \
    -a text \
    -s 'The Haskcat program'
    -d 'Probably a pure Haskell implementation of cat, I guess?'
```

Create a library `make-it-so` which exposes the module
`Web.Make.It.So` and which depends on `warp` and `wai`, and then
create three executables as well:

```bash
$ charter library make-it-so \
    -m Web.Make.It.So
    -a warp
    -a wai
    -b do-this
    -b do-that
    -b do-the-other
```
