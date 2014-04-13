;;; Provides the functions for the evaluation thread.
;;;
;;; In the normal implementation of swank (so, the common lisp one, or even the
;;; clojure one), the evaluation thread only evaluates stuff. However, in this
;;; version, the evaluation thread does more: basically all the things pass
;;; through this thread, since it's the only one that can access the namespace
;;; currently in use.

#lang racket
(require racket/base
         racket/rerequire
         (only-in srfi/13 string-prefix-ci?)
         "complete.rkt"
         "util.rkt")

(provide swank-evaluation)

(define (swank-evaluation parent-thread)
  ;; we don't use make-evaluator in racket/sandbox because we can assume
  ;; to trust the user herself (that is, ourselves, since it's mainly
  ;; run locally)
  (parameterize ([current-namespace (make-base-namespace)])
                (continuously 
                  (dispatch-eval parent-thread (thread-receive)))))

(define (pprint-eval-result res)
  (cond ([void? res] "; No value")
        ([exn? res]
         ;; the exception has already been handled. We should only print it
         (string-append (exn-message res)
                        (if (exn:srclocs? exn) 
                          (exn:srclocs-accessor exn)
                          "")))
        (else 
          (~s res #:max-width 100))))


(define (string->datum string-sexp)
  (let ([in (open-input-string string-sexp)])
   ((current-read-interaction) (object-name in) in)))

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

(define (send-back-to thread data cont)
  (thread-send
    thread
    (list 'return `(:return (:ok ,data) ,cont))))

(define (dispatch-eval pthread cmd)
  (match cmd
         [(list 'eval string-sexp cont)
          ;; string-sexp is, as the name suggest, a string version of a sexp
          ;; so, first thing, we have to trim and read it.
          (let ([stripped (trim-code-for-eval string-sexp)])
            (when (not (string=? stripped ""))
              (let* ([stx (string->datum stripped)]
                     [results 
                      (with-handlers ([exn:fail? (lambda (exn) exn)])
                                     (eval stx))])
                (thread-send pthread (list 'eval-result 
                                           (pprint-eval-result results)
                                           cont)))))]

         [(list 'complete pattern cont)
          ;; now, there is a problem:
          ;; if you define a function in the repl, the auto-completer finds it
          ;; even in the racket buffer. When you load the buffer with
          ;; `compile and load`, you get an error (if you called that
          ;; function), since the namespace used for compilation is different
          ;; from the one used to do the rest of the things. 
          (send-back-to pthread
                        (list (simple-complete pattern) pattern)
                        cont)]

         [(list 'expand times string-form cont)
          (let ([form (string->datum string-form)])
            (send-back-to
              pthread
              (pprint-eval-result
                (syntax->datum
                  ((if (= times 1) expand-once expand) form)))
              cont))]
         
         [(list 'arglist fnsym cont)
          (let ([fnobj (with-handlers 
                         ([exn:fail? (lambda (exn) #f)])
                         (namespace-variable-value fnsym))])
            (if fnobj
              (let* ([fnarity (procedure-arity fnobj)]
                     [prntarity (make-string-from-arity fnarity)])
                (send-back-to pthread prntarity cont))
              (send-back-to pthread "([x])" cont)))]

         [(list 'compile modname load? cont)

          (with-handlers
            ([exn:fail?  
              ;; well, yes, we could be a little more specific, but there
              ;; are many ways in which the compilation process may fail:
              ;; we will handle them in the `build-error-message`.
              ;; use time-apply to get the compilation time.
              (lambda (exn) 
                (thread-send 
                  pthread 
                  (list 'return 
                        `(:return (:ok (:compilation-result 
                                         ,(list (build-error-message exn))
                                         nil 0.0 nil nil))
                          ,cont))))])

            (let-values ([(_ time __ ___) 
                          (time-apply
                            dynamic-rerequire
                            (list (string->path modname)))])
                        (thread-send
                          pthread
                          (list 'return
                                `(:return (:ok (:compilation-result nil t 
                                                ,(/ time 1000.0) nil nil))
                                  ,cont))))
            (current-namespace (module->namespace (string->path modname))))]))

(define (build-error-message exn)
  `(:message ,(exn-message exn)
    :severity ,(if exn:fail:syntax? ':read ':error)
    :location ,(build-source-error-location exn)))

(define (build-source-error-location e) 
  '(:error "No source location"))

(define (make-string-from-arity fnarity)
  (define (prototype-from-int int)
    ;; unluckly, racket doesn't provide a way to get the argument list
    ;; (i.e. ccl:arglist or clojure's metadata of the variable). 
    ;; Therefore, we will invent the names of the arguments
    ;; well, geiser *does* contain the code to do that, so I will look into it.
    ;; even though it seems that it parses the code, so for now I will 
    ;; just be happy with this little hack.
    (string-join
      (map string
           (map integer->char 
                (map 
                  (lambda (x) (+ x 97)) ;; let's just use letters from #\a
                  (range 0 int))))))
  (cond ([exact-nonnegative-integer? fnarity] 
         (string-append "(" (prototype-from-int fnarity) ")"))
        ([arity-at-least? fnarity] 
         (let ([args (prototype-from-int (arity-at-least-value fnarity))])
          (string-append "("
                        args
                         (if (string=? args "")
                           ""
                           " ")
                         "...)")))
        (else ;; it's a list of possible arities: which to show?
          "([x])")))
