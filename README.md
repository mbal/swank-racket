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

- Basic evaluation and code completion works
- Only tested with Vim & SLIMV - Emacs & SLIME testers needed!

### Missing
In a rough order of importance.

- Compile
- Load (in Racket, the correct way is using `(enter! filename)`
- Eldoc-like functions prototypes in the minibuffer
- Documentation access
- All the debugging stuff (macroexpand, ...)
- Xref

## Howto

### Start the server
To start the server, just run `racket servermb.rkt`. The main program starts a
server on 127.0.0.1:4005, and you can connect with it either with emacs (`M-x
slime-connect`) or SLIMV (load a scheme file and `<leader>c`).
When the connection is successful, a REPL is started.
Multiple clients are not supported.

### The REPL
The REPL is a more or less like racket's built-in. 

Developed using Racket 6.0 (and tested under 5.93, should work on every
reasonably recent version). 

Tested under Windows:
* (g)Vim 7.4 + Python2.7 + SLIMV 0.9.12
* emacs 24.2 + slime (very briefly)

Under emacs it's very unstable (mainly *slow*), since it's still incomplete. It's better under slimv.

### License

Eclipse Public License - v 1.0
