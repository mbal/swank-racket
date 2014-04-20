swank-racket
============

This is a fork of the
[swank-racket](https://github.com/dkvasnicka/swank-racket) project by
dkvasnicka.
It's a Swank server for the [Racket](http://racket-lang.org) programming
language (formerly PLT Scheme).

Swank server is what gives you an interactive Lisp / Scheme environment when
working with SLIME (Emacs) or SLIMV (Vim).

### Status & limitations
Most of the basic stuff now works:

- The REPL
- basic completion
- Compile & Load
- Macroexpansion
- Almost all of the `Evaluation` menu (defun, current-expression, region, buffer, interactive)

### Missing
In a rough order of importance.

- Documentation access
- All the debugging stuff
- Xref

## Howto

### Start the server
To start the server, just run `racket server.rkt`. The main program starts a
server on 127.0.0.1:4005, and you can connect with it either with emacs (`M-x
slime-connect`) or SLIMV (load a racket file and `<leader>c`).
When the connection is successful, a REPL is started.
Multiple clients are not supported.

### The REPL
The REPL is a more or less like racket's built-in. You can type expressions,
followed by enter. Racket will reply with the result of the evaluation.
In the minibuffer you'll see information about the arities of the function
you're about to call. 

For example: if you write `(car` in the REPL, the minibuffer will show `(car
a)`. This happens also with user defined functions.

#### Restarts that aren't
Users of Common LIsp and SLIME are familiar with the concept of restarts. No
such thing exists in racket (it can be implemented, though, `call/cc` and the
like): in swank-racket we choose to simply abort. 
Maybe, in the future, Racket will provide something similar to conditions out
of the box.

#### Special variables
Slime provides `* ** *** / // /// + ++ +++` to get recent values. In Scheme,
all these symbols are legal names (and may be "common" choices for some
operator, e.g. `++` as an alias for append). In swank-racket, we use `*1, *2,
*3 *e`. (these too are legal names, but less common; and they're used in
swank-clojure too)
In particular:
* `*1` gets the most recent result
* `*2` second most recent
* `*3` third most recent
* `*e` last expression evaluated.

### File loading
Compile and Load is the command you're looking for. The file is loaded and the
current namespace is switched to the one in the current file. It's basically
equivalent to racket's `enter!`. Any error is shown during the compilation
process.

#### Vim & Emacs differences
Note that SLIMV's `Compile-File` (mapped to `<leader>F`) is functionally
equivalent to `Compile-Load-File` (mapped to `<leader>L`), because it sends
exactly the same command.
With emacs, it works as expected: with `Compile-file` the file is compiled, but
the functions are not available in the REPL. `Compile-Load-File` compiles and
enters the module.

Developed using Racket 6.0 (and tested under 5.93, should work on every
reasonably recent version). 

Tested under Windows:
* (g)Vim 7.4 + Python2.7 + SLIMV 0.9.12
* emacs 24.2 + slime20130402

### Differences with the original version by dkvasnicka

* This version works on windows
* Currently, it has more features
* I don't use the sandbox to evaluate the code.

### License

Eclipse Public License - v 1.0
