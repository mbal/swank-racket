;;; Various utility functions.
;;; 
#lang racket
(require racket/base
         (only-in srfi/13 string-prefix-ci?))

(provide (contract-out
           [trim-code-for-eval (-> string? string?)]
           [swank-serialize (-> list? string?)])
         schemify-truth-values
         continuously)

;; transport layer stuff
(define (swank-serialize msg)
  (let ([stringified (~s msg)])
   (string-append (~r (+ (string-length stringified) 1) 
                      #:base 16 #:min-width 6 #:pad-string "0") 
                  stringified "\n")))

(define-syntax continuously
  (syntax-rules ()
                ((continuously f)
                 (let loop ()
                  f
                  (loop)))))

(define (trim-code-for-eval c)
  ;; Trims the code entered in the repl for the evaluation. 
  ;; - removes extraneous whitespaces at the beginning or end of the string
  ;; - removes the "#lang ..." directive present when the code is loaded
  ;;   with load-buffer
  (define (strip-lang-directive code)
    (string-join
      (filter (curry (compose1 not string-prefix-ci?) "#lang")
              (string-split code "\n"))))
  (strip-lang-directive (string-trim c #:repeat? #t)))

(define (schemify-truth-values data)
  ;; changes truth values from lisp to scheme (nil -> #f, t -> #t).
  ;; It leaves the rest unchanged.
  (cond ([null? data] '()) 
        ([eq? (car data) 'nil] (cons #f (schemify-truth-values (cdr data))))
        ([eq? (car data) 't] (cons #t (schemify-truth-values (cdr data))))
        ([list? (car data)]
         (cons (schemify-truth-values (car data))
               (schemify-truth-values (cdr data))))
        (else (cons (car data) (schemify-truth-values (cdr data))))))

