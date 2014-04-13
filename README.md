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
Here's a list of what is working:

- The REPL
- basic completion (in vim, emacs not yet)
- Compile & Load
- Macroexpansion

### Missing
In a rough order of importance.

- Documentation access
- All the debugging stuff
- Xref

## Howto

### Start the server
To start the server, just run `racket servermb.rkt`. The main program starts a
server on 127.0.0.1:4005, and you can connect with it either with emacs (`M-x
slime-connect`) or SLIMV (load a scheme file and `<leader>c`).
When the connection is successful, a REPL is started.
Multiple clients are not supported.

### The REPL
The REPL is a more or less like racket's built-in. You can type expressions,
followed by enter. Racket will reply with the result of the evaluation.
In the minibuffer you'll see information about the arities of the function
you're about to call. 

For example: if you write `(car` in the REPL, the minibuffer will show `(car
a)`. This happens also with user defined functions.

### File loading
Compile and Load is the command you're looking for. The file is loaded and the
current namespace is switched to the one in the current file. It's basically
equivalent to racket's `enter!`. Any error is shown during the compilation
process.

Developed using Racket 6.0 (and tested under 5.93, should work on every
reasonably recent version). 

Tested under Windows:
* (g)Vim 7.4 + Python2.7 + SLIMV 0.9.12
* (briefly) emacs 24.2 + slime

### License

Eclipse Public License - v 1.0
