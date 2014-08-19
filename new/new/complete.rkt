;;; Contains the code for the autocompletion stuff.
;;; 
#lang racket
(require racket/base
         (only-in srfi/13 string-prefix-ci?))
(provide simple-complete)

;; for now, let's just be happy with a `starts-with` completion.
;; TODO: fuzzy completion (or at least, a bit smarter than this)
(define (simple-complete pattern)
  (let ([candidates (filter 
                      (curry string-prefix-ci? pattern) 
                      (currently-defined-symbols))])
   (if (empty? candidates) 'nil (sort candidates < #:key string-length))))

(define (currently-defined-symbols)
  (map symbol->string (namespace-mapped-symbols)))
