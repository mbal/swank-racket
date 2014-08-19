#lang racket

;; contains some utility code to handle modules, such as printing names
;; Most of it is copied from xrepl.

(require setup/path-to-relative)
(provide get-prefix)

(define (get-prefix)
  (define (module-name)
    (let* ([x (here-source)]
           [x (and x (module-displayable-name (if (symbol? x) `',x x)))])
      (string->symbol x)))
  (module-name))

(define (here-source) ; returns a path, a symbol, or #f (= not in a module)
  (variable-reference->module-source
   (eval (namespace-syntax-introduce
          (datum->syntax #f `(,#'#%variable-reference))))))

(define (->relname x)
  (relative-path-pwd (normal-case-path x)))

(define home-dir (find-system-path 'home-dir))

(define (relative-path-pwd x)
  (path->string (find-relative-path (normal-case-path (current-directory)) x)))

;;; copied from xrepl
(define (module-displayable-name mod)
  (define (choose-path x)
    ;; choose the shortest from an absolute path, a relative path, and a
    ;; "~/..." path.
    (if (not (complete-path? x)) ; shouldn't happen
      x
      (let* ([r (path->string (find-relative-path (current-directory) x))]
             [h (path->string (build-path (string->path-element "~")
                                          (find-relative-path home-dir x)))]
             [best (if (< (string-length r) (string-length h)) r h)]
             [best (if (< (string-length best) (string-length x)) best x)])
        best)))
  (define (get-prefix* path)
    (define x (if (string? path) path (path->string path)))
    (define y (->relname path))
    (if (equal? x y)
      (format "~s" (choose-path x))
      (regexp-replace #rx"[.]rkt$" y "")))
  (let loop ([mod mod])
    (match mod
      [(? symbol?) (symbol->string mod)]
      [(list 'quote (? symbol? s)) (format "'~a" (loop s))]
      [(list 'file (? string? s)) (loop (string->path s))]
      [(or (? path?) (? string?)) (get-prefix* mod)]
      [_ (error 'xrepl "internal error; ~v" mod)])))
