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
         "repl.rkt"
         "modutil.rkt"
         "util.rkt")

(provide swank-evaluation)

(define (swank-evaluation parent-thread)
  ;; we don't use make-evaluator in racket/sandbox because we can assume
  ;; to trust the user herself (that is, ourselves, since it's mainly
  ;; run locally).
  ;; There are two namespaces that share the module `repl.rkt`: the one
  ;; in which the REPL runs and the namespace of this module.
  (let ([new-ns (make-base-namespace)])
   (namespace-attach-module
     (namespace-anchor->empty-namespace repl-anchor)
     (string->path "repl.rkt")
     new-ns)
   (parameterize ([current-namespace new-ns])
                 (namespace-require "repl.rkt")
                 (continuously
                   (dispatch-eval parent-thread (thread-receive))))))

(define (dispatch-eval pthread cmd)
  (match cmd
         [(list 'eval string-sexp cont)
          (let ([stripped (trim-code-for-eval string-sexp)])
            (when (not (string=? stripped ""))
              (let* ([stx (string->datum stripped)]
                     [output-port (open-output-string)]
                     [result (try-eval stx output-port)]
                     [output (get-output-string output-port)])
                (when (not (string=? output ""))
                  (thread-send pthread (list 'return `(:write-string ,output))))
                (thread-send
                  pthread
                  (list 'return `(:return ,result ,cont))))))]

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
              (send-back-to pthread 'nil cont)))]

         [(list 'undefine-function fname cont)
          (let ([fnsym (string->symbol fname)])
            (if (namespace-variable-value fnsym #t (lambda () #f))
              (begin (namespace-undefine-variable! (string->symbol fname))
                     (send-back-to pthread fname cont))
              (send-back-to pthread 'nil cont)))]

         [(list 'describe strsym cont)
          ;; TODO
          (send-back-to pthread 'nil cont)]

         [(list 'compile modname load? cont)
          ;(compile modname load?)
          (with-handlers
            ([exn:fail?
              ;; well, yes, we could be a little more specific, but there
              ;; are many ways in which the compilation process may fail:
              ;; we will handle them in the `build-error-message`.
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
            (when load?
              ;; enter the namespace of the module
              (current-namespace (module->namespace
                                   (string->path modname)))

              ;; notify slime that we are in a new `package`
              (thread-send
                pthread
                (list 'return `(:new-package ,modname ,(get-prefix))))

              ;; we have to require again repl.rkt in order to access
              ;; the * variables.
              (namespace-require "repl.rkt")))]))

(define (try-eval stx out)
  ;; Wraps evaluation of `stx` in a try/except block and redirects the
  ;; output to `out`. Returns the correct message to be sent to emacs.
  (with-handlers
    ([exn:fail?
      (lambda (exn) `(:abort ,(print-exception exn)))])

    (let ([result (parameterize ([current-output-port out])
                                (eval stx))])
      ;; variables in a module can be updated only from within the 
      ;; module itself.
      (update-vars! result *1 *2 (syntax->datum stx))
      `(:ok (:values ,(pprint-eval-result result))))))

(define (build-error-message exn)
  (displayln exn)
  (flush-output (current-output-port))
  `(:message ,(exn-message exn)
    ;; TODO: possibilities for severity are: :error, :read-error, :warning
    ;; :style-warning :note :redefinition. Probably all but the first two are
    ;; useless in Racket. I still need to figure out a way to get the list
    ;; of all errors in the racket module being compiled.
    :severity ,(if (exn:fail:read? exn) ':read-error ':error)
    :location ,(build-source-error-location exn)))

(define (build-source-error-location e)
  (if (exn:srclocs? e)
    (let ([srclcs (car ((exn:srclocs-accessor e) e))])
     `(:location
        ;; TODO: i have to check this: reading slime's source it seems that
        ;; either (:file :line) or (:buffer :offset) should be present.
        ;;
        ;; XXX: aside, there's a small bug in this paredit, ( in comments are
        ;; highlighted with ) not in comments.
        (:file ,(path->string (srcloc-source srclcs)))
        (:position ,(srcloc-position srclcs))
        (:line ,(srcloc-line srclcs))))
    '(:error "No source location")))

(define (make-string-from-arity fnarity)
  ;; we use this function with swank:operator-arglist it's not very smart, and 
  ;; there isn't a nice way to handle functions with optional arguments
  ;; SLIMV uses only swank:operator-arglist, but slime doesn't send that
  ;; message, it prefers a more advanced version, which allows us to check with
  ;; more precision which arity to show (and highlight arguments in the
  ;; minibuffer)
  (define (prototype-from-int int)
    ;; unluckily, racket doesn't provide a way to get the argument list
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
                         (if (string=? args "") "" " ")
                         "...)")))
        (else ;; it's a list of possible arities: which to show?
          "([x])")))

(define (pprint-eval-result res)
  (if (void? res)
    "; No value"
    (~s res #:max-width 100)))

(define (print-exception exn)
  ;; the exception has already been handled. We should only print it
  (string-append (exn-message exn) 
                 (if (exn:srclocs? exn)
                   (srcloc-position (car ((exn:srclocs-accessor exn) exn)))
                   "")))

(define (string->datum string-sexp)
  (let ([in (open-input-string string-sexp)])
   ((current-read-interaction) (object-name in) in)))

(define (send-back-to thread data cont)
  (thread-send
    thread
    (list 'return `(:return (:ok ,data) ,cont))))

(define (multiple-nsvv vars vals)
  (map namespace-set-variable-value! vars vals))

