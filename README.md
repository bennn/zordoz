Zordoz
======

[ZORDOZ](https://www.youtube.com/watch?v=kbGVIdA3dx0) speaks to you! His chosen ones.


This is an explorer for Racket .zo files.

Usage
-----

Run `make` to create an executable called `zordoz`.
You can run this executable by giving it a `.zo` bytecode file: `./zordoz FILE.zo`.

The REPL accepts the following commands:

- `alst` prints all command aliases; for example, the repl treats the words 'alst' and 'aliases' the same way
- `back` goes back to the previous context
- `dive ARG` changes context. For any `<struct:val>` printed by `info`, you can `dive val`. Also, you can `dive i` if `info` prints a list with at least `i` elements.
- `find ARG` searches for matches to `ARG` and, if successful, changes context to the list of results
- `help` prints information about these commands
- `info` prints data about the current context
- `jump` reverts to a previously saved context
- `save` marks the current context as a target for `jump`
- `quit` exits the interpreter

To run tests, execute `make test`.
Each module in the `private/` folder contains a module `test+` containing unit tests.

The functions implementing the `dive`, `find`, and `info` commands are available outside the REPL.
Check the [guide](http://bennn.github.io/zordoz) for a summary.

Background
----------

Racket bytecode is stored in files with a `.zo` extension [1](http://docs.racket-lang.org/raco/make.html).
This tool makes it easier to explore the bytecode representation of a file, whether or not you have access to the file's source code.

Given a `.zo` file, we decompile the bytecode into a struct (aka, a "zo-struct") using Racket's built-in decompilation API [2](http://docs.racket-lang.org/raco/decompile.html).
The REPL loads this struct as its initial _context_ and begins accepting commands, making it easy to visualize and explore Racket bytecode.

Example
-------

Suppose we create and compile a small racket file:
```
> echo -e "#lang racket/base\n(if #t (+ 1 1) 0)" > test.rkt
> raco make test.rkt
```

The actual bytecode is not human readable.
Neither is the struct representation output by `zo-parse`:
```
> echo -e '#lang racket/base\n(require compiler/zo-parse)\n(call-with-input-file "compiled/test_rkt.zo"\n  (lambda (fd) (displayln (zo-parse fd))))' > print-test.rkt
> racket print-test.rkt
#s((compilation-top zo 0) 0 #s((prefix zo 0) 0 (#f) ()) #s((mod form 0 zo 0) test test #<module-path-index> #s((prefix zo 0) 0 (#s((module-variable zo 0) #<module-path-index> print-values 0 0 #s((function-shape zo 0) #(struct:arity-at-least 0) #f))) ()) ((0 () ()) (1 () ()) (#f () ())) ((0 #<module-path-index>) (1) (-1) (#f)) (#s((apply-values expr 0 form 0 zo 0) #s((toplevel expr 0 form 0 zo 0) 0 0 #t #t) 2)) () ((0 () ())) 0 #s((toplevel expr 0 form 0 zo 0) 0 0 #f #f) #f #t () (#s((mod form 0 zo 0) (test configure-runtime) configure-runtime #<module-path-index> #s((prefix zo 0) 0 (#s((module-variable zo 0) #<module-path-index> configure 0 0 #s((function-shape zo 0) 1 #f))) ()) ((0 () ()) (1 () ()) (#f () ())) ((0 #<module-path-index> #<module-path-index>) (1) (-1) (#f)) (#s((application expr 0 form 0 zo 0) #s((primval expr 0 form 0 zo 0) 1000) (#t))) () ((0 () ())) 1 #s((toplevel expr 0 form 0 zo 0) 0 0 #f #f) #f #t () () ())) ()))
```

ZORDOZ offers a more readable presentation.
Below is a sample interactive session with the same small file (interspersed with commentary):

```
> racket zordoz.rkt compiled/test_rkt.zo 
INFO: Loading bytecode file 'compiled/test_rkt.zo'...
INFO: Parsing bytecode...
INFO: Parsing complete!
--- Welcome to the .zo shell, version 0.1 'outlands' ---
zo> info
<struct:compilation-top>
  max-let-depth : 0
  prefix        : <struct:prefix>
  code          : <struct:mod>
```

The `compilation-top` struct is at the top of most every `.zo` file.
Things get more interesting as we explore the structs nested inside it.

```
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
```

The `mod` struct represents a Racket module.
This module has the name `test`; inferred from our filename `test.rkt`.

We could continue `dive`-ing into structs, or we can use the shell's `find` command to look for structs matching a name like `mod` or `compilation-top`.
Let's search for `branch` structs.
Maybe we can find the `if`-statement in our original code.

```
zo> find branch
FIND returned 0 results
```

Nothing.
The `if`-statement has been optimized away.
Let's try to find what it turned into by searching the body of the module.

```
zo> dive body
zo> info
(<struct:apply-values>)[1]
```

The syntax `(<struct:NAME>)[LENGTH]` denotes a list of zo-structs.
`LENGTH` is the number of elements in the list--we can `dive` into any valid index.

```
zo> dive 0
zo> info
<struct:apply-values>
  proc      : <struct:expr>
  args-expr : 2
```

Looks like our `if`-statement was optimized into a constant, `2`.

Happy exploring!
