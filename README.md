Zordoz
======

ZORDOZ speaks to you! His chosen ones.


This is an analyzer for Racket .zo files.

Usage
-----

Run `make` to create an executable called `zordoz`.
You can run this executable by giving it a `.zo` bytecode file: `./zordoz FILE.zo`.

Inside the REPL:

- `back` goes back to the previous context
- `dive ARG` changes context. For any `<struct:val>` printed by `info`, you can `dive val`. Also, you can `dive i` if `info` prints a list with at least `i` elements.
- `find ARG` searches for matches to `ARG` and, if successful, changes context to the list of results.
- `help` prints information about these commands
- `info` prints data about the current context
- `quit` exits the interpreter

To run tests, execute `make test`.
Each module in the `src/` folder contains a submodule `test+` containing unit tests.

Background
----------

Racket bytecode is stored in files with a `.zo` extension [1](http://docs.racket-lang.org/raco/make.html).
This tool makes it easier to explore the bytecode representation of a file, whether or not you have access to the file's source code.

Given a `.zo` file, we decompile the bytecode into a struct (aka, a "zo-struct") using Racket's built-in decompilation API [2](http://docs.racket-lang.org/raco/decompile.html).
The REPL loads this first struct as its context and then prints a human-readable representation.
From there, one can see the names and representations of each field of the struct.
If a field points to a value that is not a zo-struct, then the entire value is printed.
Otherwise, only the name of the zo-struct is shown; however, the REPL can change focus to that struct.

Additionally, the `find` command will search the current struct and its children for a particular zo-struct.
Successful queries return a list of results; these results can be explored individually by calling `dive i` for an index `i` into the list.

Example
-------

Below you can see the results of creating and exploring a small file.

```
> echo -e "#lang racket/base\n(if #t (+ 1 1) 0)" > test.rkt
> raco make test.rkt
> racket main.rkt compiled/test_rkt.zo 
INFO: Loading bytecode file 'compiled/test_rkt.zo'...
INFO: Parsing bytecode...
INFO: Parsing complete!
--- Welcome to the .zo shell, version 0.1 'outlands' ---
zo> info
<struct:compilation-top>
  max-let-depth : 0
  prefix        : <struct:prefix>
  code          : <struct:mod>
zo> dive code
zo> info
<struct:mod>
  name             : test
  srcname          : test
  self-modidx      : #<module-path-index>
  prefix           : <struct:prefix>
  provides         : 0 [] [] 1 [] [] #f [] []
  requires         : 0 #<module-path-index> 1  -1  #f 
  body             : <struct:apply-values>
  syntax-bodies    : 
  unexported       : 0  
  max-let-depth    : 0
  dummy            : <struct:toplevel>
  lang-info        : #f
  internal-context : #t
  flags            : 
  pre-submodules   : <struct:mod>[1]
  post-submodules  : []
zo> find branch
FIND returned 0 results
zo> dive body
zo> info
(<struct:apply-values>)[1]
zo> dive 0
zo> info
<struct:apply-values>
  proc      : <struct:expr>
  args-expr : 2
```

As you can see, the branch `(if #t ...)` has been optimized away.
Instead, there is a list of `apply-values` structs.
We explore the first element of this list with the `dive 0` command and see that the call to `+` in our source code has been replaced with the constant `2`.
