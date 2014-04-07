#lang racket
(require racket/base)
(provide swank-serialize
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

