#lang racket

(require 
  racket/tcp
  racket/sandbox
  racket/base
  (only-in srfi/13 string-prefix-ci?)
  "getpid.rkt"
  "util.rkt")

(define listener (tcp-listen 4005 5 #t))
(displayln "Running racket-swank on port 4005")

(define racket-base-symbols
  (let-values ([(procs1 procs2) (module->exports 'racket)])
              (map symbol->string 
                   (filter symbol? (flatten (append procs1 procs2))))))

;; for now, let's just be happy with a `starts-with` completion.
;; TODO: fuzzy completion (or at least, a bit smarter than this)
(define (code-complete pattern)
  (let ([candidates 
         (filter (curry string-prefix-ci? pattern) racket-base-symbols)])
   (if (empty? candidates) 'nil candidates)))

(define (start-server)
  (displayln "Waiting for the client to connect")
  (define-values (i o) (tcp-accept listener))
  (displayln "Client connection accepted!")

  (define control-thread/mb
    (thread (lambda () (control-loop/mb i o))))

  ;; the main thread reads from the connection and puts data in the
  ;; control thread's mailbox.
  (continuously 
    (begin
      (read i)
      (thread-send control-thread/mb (read i))))

  (thread-wait control-thread/mb)
  (display "server ended"))

(define (swank-evaluation parent-thread)
  ;; we don't use make-evaluator in racket/base because we can assume
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

(define (dispatch-eval pthread cmd)
  (match cmd
         [(list 'eval string-sexp cont)
          ;; string-sexp is, as the name suggest, a string version of a sexp
          ;; so, first thing, we have to trim and read it.
          (let ([stripped (string-trim string-sexp #:repeat? #t)])
            (when (not (string=? stripped ""))
              (displayln (current-namespace))
              (let* ([in (open-input-string string-sexp)]
                     [stx ((current-read-interaction) (object-name in) in)]
                     [results 
                      (with-handlers ([exn:fail? (lambda (exn) exn)])
                                     (eval stx))])
                (thread-send pthread (list 'eval-result 
                                           (pprint-eval-result results)
                                           cont)))))]
         [(list 'arglist fnsym cont)
          (let ([fnobj (with-handlers 
                         ([exn:fail? (lambda (exn) #f)])
                         (namespace-variable-value fnsym))])
            (if fnobj
              (let* ([fnarity (procedure-arity fnobj)]
                     [prntarity (make-string-from-arity fnarity)])
                (thread-send pthread 
                             (list 'return
                                   `(:return (:ok ,prntarity) ,cont))))
              (thread-send pthread
                           (list 'return `(:return (:ok "([x])") ,cont)))))]))

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

(define (control-loop/mb input output)
  (continuously (dispatch-event (thread-receive) output)))

(define (get-thread t parent)
  (cond ((eq? t 't) 'fail)
        ((eq? t ':repl-thread)
         (if repl-thread
           repl-thread
           (spawn-repl-thread parent)))))

(define repl-thread #f)
(define (spawn-repl-thread parent)
  (let ([thr (thread (lambda () (swank-evaluation parent)))])
   (set! repl-thread thr)
   thr))

(define (write-to-connection data out)
  (displayln data)
  (flush-output (current-output-port))
  (display (swank-serialize data) out)
  (flush-output out))

(define (dispatch-event data out)
  (let ([action (car data)])
   (cond ([eq? action ':emacs-rex] 
          (handle-emacs-command data))
         ([eq? action 'return]
          (write-to-connection
            (cadr data) out))
         ([eq? action 'eval-result]
          (write-to-connection 
            `(:return (:ok (:values ,(cadr data))) ,(caddr data))
            out))
         ([#t (display "not yet supported")]))))


(define (handle-emacs-command data) 
  ;; This function is kind of ugly, because it matches all the possible
  ;; messages and dispatches them to the correct thread. 
  ;; Note that while slime and other implementations (such as clojure-swank)
  ;; spawn a new thread for each command, I chose not to do so for the 
  ;; simplest messages.
  ;; So, when we receive a "simple" message (i.e. a message that doesn't 
  ;; require much computation) we answer directly, but we mimick being
  ;; another thread: we put the response in the mailbox of *this same thread*.
  ;; Then, the control thread will retrieve the response (that it put there)
  ;; and send it on the wire.
  (displayln data)
  (let ([cmd (cadr data)]
        ;; It's mainly "Racket" ?
        [ns (caddr data)]
        ;; possibile values are: t (new thread) or repl-thread 
        [thread (cadddr data)] 
        ;; increasing numbers
        [cont (last data)])
    (match cmd
           [(list 'swank:connection-info)
            (thread-send 
              (current-thread)
              (list 'return
                    `(:return (:ok (:pid ,(getpid)
                                    :package (:name racket :prompt racket)
                                    :encoding (:coding-systems ("utf-8-unix"))
                                    :lisp-implementation
                                    (:type "Racket" :version ,(version)))) 
                      ,cont)))]
           [(list 'swank:swank-require _)
            (thread-send 
              (current-thread)
              (list 'return `(:return (:ok nil) ,cont)))]
           [(list 'swank:create-repl _ ...)
            (thread-send
              (current-thread)
              (list 'return `(:return (:ok ("racket" "racket")) ,cont)))]
           [(list 'swank:listener-eval code) 
            (let ([eval-thread (get-thread thread (current-thread))])
              (thread-send eval-thread (list 'eval code cont)))]
           [(list 'swank:simple-completions pattern _) 
            (thread-send
              (current-thread)
              (list 'return 
                    `(:return (:ok ,(list (code-complete pattern) pattern)) 
                      ,cont)))]

           [(list 'swank:operator-arglist fn a)
            (let ([eval-thread (get-thread ':repl-thread (current-thread))])
              (thread-send eval-thread 
                           (list 'arglist (string->symbol fn) cont)))]

           [(list 'swank:compile-file-for-emacs fname load?)
            (thread-send
              (current-thread)
              (list 'return 
                    `(:return 
                       (:ok (:compilation-result nil t 0.0 nil nil)) 
                       ,cont)))]
           [_ (thread-send
                (current-thread)
                (list 'return `(:return (:ok nil) ,cont)))])))

(start-server)
