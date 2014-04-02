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

### Howto

Developed using Racket 6.0 (and tested under 5.93, should work on every
reasonably recent version). Tested under SLIMV, Python27 and Windows.
Run `racket server.rkt` to get the thing up and running.

### License

Eclipse Public License - v 1.0
