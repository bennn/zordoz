#lang scribble/manual

@local-table-of-contents[]

@title{REPL}

The REPL is a simple, interactive way to explore bytecode files.
It is implemented in the file @tt{zo-shell.rkt}, which exports a single function:

@defproc[(init [args (vectorof string?)]) void?]{
  Accepts a @tt{vector} of command-line arguments.
  @itemlist[
    @item{If given exactly one argument, starts a REPL session using the output of calling @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28def._%28%28lib._compiler%2Fzo-parse..rkt%29._zo-parse%29%29"]{zo-parse} on this file.}
    @item{If given more than one argument, parses the first argument as a bytecode file and the rest as strings. Searches the parsed bytecode file for structs matching the strings using @code{zo-find}.}
  ]
}

This document is a users' guide for the REPL.
See the @secref{API} page for implementation details.

@section{Summary}

The REPL works by storing an internal @emph{context} and reacting to commands.
This context is normally a zo struct, but may also be:
@itemlist[
  @item{A list of zo structs}
  @item{Search results, obtained by calling @secref{find}.}
]

The commands show information about this context or change it.

Also, the REPL remembers previous states.
We call this recorded past the @emph{history} of the REPL.
It's basically a list of contexts.


@section{Commands}
The REPL supports commands to display and explore structs parsed from a @tt{.zo} file.

@subsection{@bold{alst}}

List all command aliases.

For fun and uniformity, the canonical name of each command has 4 letters.
But each has plenty of shorter and more mnemonic aliases to choose from.
For example, you can type @tt{ls} instead of @secref{info} and @tt{cd} instead of @secref{dive}.


@subsection{@bold{back}}

Move up to the previous context.

Each successful @secref{dive} or @secref{find} command changes the current context to new struct or list.
Before making these transitions, we save the previous context to a stack.
The @secref{back} command pops and switches to the most recent element from this stack.

Note that @secref{back} will fail (and do nothing) at the top of the zo struct hierarchy or the top of a saved subtree.


@subsection{@bold{dive}}

Enter a struct's field.

This is where exploring happens.
Each struct has a few fields; you can see these by printing with @secref{info}.
Any field containing zo structs is a candidate for dive.
For example, the struct @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28def._%28%28lib._compiler%2Fzo-structs..rkt%29._assign%29%29"]{assign} has a field @tt{rhs}, which can be accessed by:
@racketblock[
  dive rhs
]

Notes:
@itemlist[
  @item{Fields that do not exist or do not contain structs may not be explored.}
  @item{Fields that are zo change the context to a zo; fields that are lists change the context to a list.}
  @item{Elements in a list may be accessed by their index, as in @secref{dive} @tt{3}.}
  @item{@secref{dive} takes exactly one argument. Any more or less is not permitted.}
]


@subsection{@bold{find}}

Search the current struct's children for a certain zo struct.

Find uses string matching to automate a simple search process.
Give it a string, for instance @secref{find} @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28def._%28%28lib._compiler%2Fzo-structs..rkt%29._lam%29%29"]{lam}, and it searches for @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28def._%28%28lib._compiler%2Fzo-structs..rkt%29._lam%29%29"]{lam} structs nested within the current context.
The string must be the name of a zo struct---anything else will return null results.

A successful find changes context to a list of zo structs.
Exploring any element of the list changes the current history to be that element's history.
You are free to explore the children and parents of any struct returned by a find query.
Use @secref{jump} to immediately return to the search results.


@subsection{@bold{help}}

Print command information.

Shows a one-line summary of each command.
The tabernacle is all-knowing.

@subsection{@bold{info}}

Print the current context.

This @secref{info} command does the real work of exploring.
It shows the current context, whether struct or list.
Lists give their length and the names of their elements.
Zo structs show their name, their fields' names, and their fields' values.
Non-struct field values are printed as best we know how; struct field values are not printed in detail, but may be @secref{dive}-ed into.

There is currently no tool for printing the current history.


@subsection{@bold{jump}}

Warp back to a previously-saved context.

The commands @secref{jump} and @secref{save} work together.
After saving or making a successful query with @secref{find}, the current history is saved.
At this point, a step backwards will recover this history.
The interesting thing is that steps forward create a new history, and you can immediately forget that new history by calling @secref{jump}.

For example, if you call @secref{find} and explore one of the results, you can immediately @secref{jump} back to your search results.


@subsection{@bold{save}}

Mark the current context and history as a future target for @secref{jump}.

Manually calling @secref{save} is not very useful, but helpful if you know of a zo struct you will want to revisit.


@subsection{@bold{quit}}

Exit the REPL.


@section{Sample Interaction}

Let's explore the REPL's own bytecode.
First, compile the project and build an executable:
@racketblock[
$ raco make zordoz.rkt
$ raco exe zordoz.rkt
]

Now we have both an executable and some bytecode to explore.
Open the bytecode for @tt{zo-string.rkt}.
You should see the interpreter prompt.

@racketblock[
$ ./zordoz private/compiled/zo-string_rkt.zo
INFO: Loading bytecode file 'private/compiled/zo-string_rkt.zo'...
INFO: Parsing bytecode...
INFO: Parsing complete!
--- Welcome to the .zo shell, version 1.0 'vortex' ---
zo>
]

Now we can start typing commands, like @secref{info}.

@racketblock[
zo> info
<struct:compilation-top>
  max-let-depth : 31
  prefix        : <struct:prefix>
  code          : <struct:mod>
]

Next, let's try a @secref{dive}.

@racketblock[
zo> dive max-let-depth
'dive max-let-depth' not permitted.
]

Didn't work!
That's because @tt{max-let-depth} is an integer.
Let's try one of the structs.

@racketblock[
zo> dive prefix
zo> info
<struct:prefix>
  num-lifts : 0
  toplevels : [#f]
  stxs      : []
]

Great!
We can't dive any further from here, so let's go back up.

@racketblock[
zo> back
zo> info
<struct:compilation-top>
  max-let-depth : 31
  prefix        : <struct:prefix>
  code          : <struct:mod>
]

And we're back to where we began.
From here we @emph{could} dive to the @tt{code} field and print it, but the printout is a little overwhelming.
The module we're exploring, @tt{zo-string}, creates over 40 different functions.
There's just a lot of data to look at, and because it's heterogenous data we do not have a nice way of truncating it.

Instead, we let's use @secref{find}.
Be warned, the search might take a minute.

@racketblock[
zo> find compilation-top
FIND returned 0 results
]

Zero results is good: there should not be any other @tt{compilation-top} structs besides the one we're currently in.
Now try searching for something else, like @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28def._%28%28lib._compiler%2Fzo-structs..rkt%29._branch%29%29"]{branch}.

@racketblock[
zo> find branch
FIND returned 422 results
FIND automatically saving context
<struct:branch>[422]
]

Wow! Over 400 results.
We can start exploring one of them:

@racketblock[
zo> dive 17
zo> info
<struct:branch>
  test : <struct:application>
  then : <struct:seq>
  else : <struct:branch>
]

We can also explore its children and parents.

@racketblock[
zo> dive test
zo> info
<struct:application>
  rator : <struct:primval>
  rands : [<struct:localref>]
zo> dive rator
zo> info
<struct:primval>
  id : 90
zo> up
zo> up
zo> info
<struct:branch>
  test : <struct:application>
  then : <struct:seq>
  else : <struct:branch>
zo> up
zo> info
<struct:branch>
  test : <struct:localref>
  then : <struct:branch>
  else : #f
]

And if we do a @secref{jump}, we return to the search results.

@racketblock[
zo> jump
zo> info
<struct:branch>[422]
]

