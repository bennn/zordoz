Zordoz
======

ZORDOZ speaks to you! His chosen ones.


This is an analyzer for Racket .zo files.

zo-shell
--------

REPL for investigating `.zo` files.
- Start with `racket main.rkt <file.zo>`
- `info` prints data about the current context
- `dive ARG` changes context. For any `<struct:val>` printed by `info`, you can `dive val`.
- `find ARG` searches for matches to `ARG` and, if successful, changes context to the list of results.
- `back` goes back to the previous context
- `help` and `quit` "do the obvious thing".
