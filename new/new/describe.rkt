#lang racket
(provide description)

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

(define (description x)
  (cond [(boolean? x) (bool-description x)]
        [(number? x) (number-description x)]
        [(string? x) (string-description x)]
        [(char? x) (char-description x)]
        [else "cannot describe (yet)"]))

(define (bool-description x)
  (format "Value: ~a (~a)\nType: boolean" x (if x "true" "false")))

(define (number-description z)
  ;; maybe we can display more infos (slime shows conversions to other bases, for ex.)
  (cond [(eqv? z +inf.0)
         (format "Value: ~s\nType: positive infinity" z)]
        [(eqv? z -inf.0)
         (format "Value: ~s\nType: negative infinity" z)]
        [(eqv? z +nan.0)
         (format "Value: ~s\nType: not-a-number" z)]
        [else (format "Value: ~s\nType: ~s" z)]))

(define (string-description x)
  (format "Value: ~a\nType: ~a string\nLength: ~a"
          x (if (immutable? x) "immutable" "mutable") (string-length x)))

(define (char-description x)
  (let ([code-point (char->integer x)])
   (format "Value: ~a\nType: character\nchar-code: ~a (#x~x)"
           x code-point code-point)))

