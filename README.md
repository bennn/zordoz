Zordoz
======

ZORDOZ speaks to you! His chosen ones.


This is an analyzer for Racket .zo files.

count-zo-structs
----------------

Contains functions for traversing a parsed bytecode file.
Basic usage:
- `raco make <file>` to generate bytecode for your favorite file
- `zo-parse <file_rkt.zo>` to load the bytecode (`(require compiler/zo-parse)`)
- `count-structs` on the output of `zo-parse` to count occurrences of all AST nodes
- `zsc->string` on the output of `count-structs` to see the counter data in `.tab` format

This is extremely slow and memory-intensive.
Use with caution.

zo-shell
--------

REPL for investigating `.zo` files.
- Start with `racket zo-shell.rkt <file.zo>`
- `info` prints data about the current context
- `dive ARG` changes context. For any `<struct:val>` printed by `info`, you can `dive val`.
- `back` goes back to the previous context

Beware, you cannot yet `dive` into lists and there are probably lots of hidden bugs (type errors).
