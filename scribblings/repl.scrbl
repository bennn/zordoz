#lang scribble/manual

@local-table-of-contents[]

@title{REPL}

The REPL is a simple, interactive way to explore bytecode files.
This document is a users' guide for the REPL.
See the @secref{API} page for alternate ways of starting a REPL (besides the command line).

@section{Summary}

The REPL works by storing an internal @emph{context} and reacting to commands.
This context is either:
@itemlist[
  @item{A zo struct}
  @item{A list of zo structs}
  @item{Search results, obtained by calling @secref{find}.}
]

The commands observe or advance this context.
Commands may be separated by newlines or semicolons.

For convenience, the REPL records previous states.
We call this recorded past the @emph{history} of the REPL; it is a stack of contexts.

Keeping this stack in mind is useful for understanding the REPL commands.


@section{Commands}

@subsection{alst}

List all command aliases.

For uniformity, the canonical name of each command has 4 letters.
But each has a few mnemonic aliases to choose from.
For example, you can type @tt{ls} instead of @secref{info} and @tt{cd} instead of @secref{dive}.


@subsection{back}

Move up to the previous context.

Each successful @secref{dive} or @secref{find} command changes the current context to new struct or list.
Before making these transitions, we save the previous context to a stack.
The @secref{back} command pops and switches to the most recent element from this stack.

Note that @secref{back} will fail (and print a warning) at the top of the zo struct hierarchy or the top of a saved subtree.


@subsection{dive}

Enter a struct's field.

This is where exploring happens.
Each struct has a few fields; you can see these by printing with @secref{info}.
Any field containing zo structs is a candidate for dive.
For example, the struct @racket[assign] has a field @tt{rhs}, which can be accessed by:
@racketblock[
  dive rhs
]

If you know where you are going, you can chain paths together.
Starting at a @racket[beg0] struct, this command moves to the first @racket[expr] or @racket[seq] in the sequence.
@racketblock[
  dive seq/0
]

Extra Notes:
@itemlist[
  @item{Only fields that contain zo structures or lists of zo structures may be explored.}
  @item{Changing to a zo structure field changes the context to the child zo structure.
        Changing to a list field changes context to that list, from which you can select a natural-number position
        in the list to explore.}
  @item{@secref{dive} takes exactly one argument. Any more or less is not permitted.}
]


@subsection{find}

Search the current struct's children for a certain zo struct.

Find uses string matching to automate a simple search process.
Give it a string, for instance @secref{find} @racket[lam] structs nested within the current context.
The string must be the name of a zo struct---anything else will return null results.

A successful find changes context to a list of zo structs.
Exploring any element of the list changes the current history to be that element's history.
You are free to explore the children and parents of any struct returned by a find query.
Use @secref{jump} to immediately return to the search results.

Note:
@itemlist[
  @item{If, after exploring a search result, you move @tt{back} past the list of search results, the REPL will print a notice.}
]


@subsection{help}

Print command information.

Shows a one-line summary of each command.
The tabernacle is all-knowing.


@subsection{info}

Print the current context.

This @secref{info} command does the real work of exploring.
It shows the current context, whether struct or list.
Lists give their length and the names of their elements.
Zo structs show their name, their fields' names, and their fields' values.

Struct fields are printed as best we can.
@itemlist[
  @item{Fields which are zo structures print their names. These fields may be @secref{dive}-ed into.}
  @item{Fields which are lists containing at least one zo structure are printed with a natural number in square braces,
        indicating the number of zo structs inside the list. These fields may also be @secref{dive}d into.}
  @item{Other fields are printed with Racket's default printer. Be aware, lists and hashes can sometimes cause very large
        printouts.}
]


@subsection{jump}

Warp back to a previously-saved context.

The commands @secref{jump} and @secref{save} work together.
After saving or making a successful query with @secref{find}, the current history is saved.
At this point, a step backwards will recover this history.
The interesting thing is that steps forward create a new history, and you can immediately forget that new history by calling @secref{jump}.

For example, if you call @secref{find} and explore one of the results, you can immediately @secref{jump} back to your search results.


@subsection{save}

Mark the current context and history as a future target for @secref{jump}.
This is useful for marking a struct you want to backtrack to.

Note that, if you manually backtrack past a @secref{save}d struct then its mark disappears and the REPL prints a notice.


@subsection{quit}

Exit the REPL.


@section{Sample Interaction}

Let's explore the REPL's own bytecode.
Starting from the directory you cloned this repo to
(or where `raco` put it on your filesystem):

@racketblock[
$ raco zordoz private/compiled/zo-string_rkt.zo
INFO: Loading bytecode file 'private/compiled/zo-string_rkt.zo'...
INFO: Parsing bytecode...
INFO: Parsing complete!
--- Welcome to the .zo shell, version 1.0 'vortex' ---
zo>
]

Now we can start typing commands, like @secref{info}.

@racketblock[
zo> info
<zo:compilation-top>
  max-let-depth : 31
  prefix        : <zo:prefix>
  code          : <zo:mod>
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
<zo:prefix>
  num-lifts : 0
  toplevels : [#f]
  stxs      : []
]

Great!
We can't dive any further from here, so let's go back up.

@racketblock[
zo> back
zo> info
<zo:compilation-top>
  max-let-depth : 31
  prefix        : <zo:prefix>
  code          : <zo:mod>
]

And we're back to where we began.
From here we @emph{could} dive to the @tt{code} field and print it, but the printout is a little overwhelming.
The module we're exploring, @tt{zo-string}, creates over 40 different functions.
There's just a lot of data to look at, and because it's heterogenous data we do not have a nice way of truncating it.

Instead, we'll try the @secref{find} command.
Be warned, the search might take a minute.

@racketblock[
zo> find compilation-top
FIND returned 0 results
]

Zero results is good: there should not be any other @tt{compilation-top} structs besides the one we're currently in.
Now try searching for something else, like @racket[branch].

@racketblock[
zo> find branch
FIND returned 422 results
FIND automatically saving context
<zo:branch>[422]
]

Wow! Over 400 results.
We can start exploring one of them:

@racketblock[
zo> dive 17
zo> info
<zo:branch>
  test : <zo:application>
  then : <zo:seq>
  else : <zo:branch>
]

We can also explore its children and parents.

@racketblock[
zo> dive test
zo> info
<zo:application>
  rator : <zo:primval>
  rands : [<zo:localref>]
zo> dive rator
zo> info
<zo:primval>
  id : 90
zo> up
zo> up
zo> info
<zo:branch>
  test : <zo:application>
  then : <zo:seq>
  else : <zo:branch>
zo> up
zo> info
<zo:branch>
  test : <zo:localref>
  then : <zo:branch>
  else : #f
]

And if we do a @secref{jump}, we return to the search results.

@racketblock[
zo> jump
zo> info
<zo:branch>[422]
]

