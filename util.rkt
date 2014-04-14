;;; Various utility functions.
;;; 
#lang racket
(require racket/base
         (only-in srfi/13 string-prefix-ci?))
(provide swank-serialize
         trim-code-for-eval
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
