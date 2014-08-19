#lang racket

(define (describe sym)
  (define b (identifier-binding (syntax sym)))
  ;; 
  (describe-binding sym b))

(define (describe-binding sym bind)
  (cond 
    [(not bind)
     (format "; `~s' is a toplevel (or unbound) identifier\n" sym)]
    [(eq? bind 'lexical)
     (format "; `~s' is a lexical identifier\n" sym)]
    [else
     ;; most interesting case
     (define-values [src-mod src-id nominal-src-mod nominal-src-id
                     src-phase import-phase nominal-export-phase]
                    (apply values bind))
     (string-append
       (format "; `~s' is a bound identifier\n" sym)
       (format "; defined in ~a~a\n" src-mod 
               (if (not (eq? sym src-id)) (format " as `~s'" src-id) "")))
     ;; other infos may be useful.
     ]))

