;;; Contains the code for the control thread.
;;;
;;; In swank, the control thread is always running, it receives all the
;;; commands and dispatches them to the correct thread. Moreoever, it collects
;;; all the results from the spawned threads.
;;;
#lang racket
(require racket/base
         (only-in srfi/13 string-prefix-ci?)
         "eval.rkt"
         "getpid.rkt"
         "util.rkt")

(provide control-loop)

(define (control-loop input output)
  (continuously (dispatch-event (thread-receive) output)))

(define repl-thread #f)

(define (spawn-repl-thread parent)
  (let ([thr (thread (lambda () (swank-evaluation parent)))])
   (set! repl-thread thr)
   thr))

(define (get-thread t parent)
  (cond ((eq? t 't) 'fail)
        ((eq? t ':repl-thread)
         (if repl-thread
           repl-thread
           (spawn-repl-thread parent)))))

(define (dispatch-event data out)
  ;; this way to handle events resembles a bit erlang's.
  (displayln data)
  (flush-output (current-output-port))
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

(define protocol-version "2013-03-08")

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
  ;; Another difference we have in this implementation is that we send
  ;; many commands to the repl-thread. For example, the command
  ;; `compile-file-for-emacs` should be dispatched to another thread, if
  ;; we follow slime's advice (the thread part of the data is set to 't)
  ;; but, in racket's namespace model, we cannot do that, since, if we 
  ;; have to load the file, we should also set it as default namespace for
  ;; the repl.
  (displayln data)
  (let* ([data (schemify-truth-values data)]
         [cmd (cadr data)]
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
                                    (:type "Racket" :version ,(version))
                                    :version ,protocol-version)) 
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
            (let ([eval-thread (get-thread ':repl-thread (current-thread))])
              (thread-send eval-thread (list 'complete pattern cont)))]

           [(list 'swank:operator-arglist fn a)
            (let ([eval-thread (get-thread ':repl-thread (current-thread))])
              (thread-send eval-thread 
                           (list 'arglist (string->symbol fn) cont)))]

           [(list 'swank:swank-macroexpand-1 form)
            (let ([eval-thread (get-thread ':repl-thread (current-thread))])
              (thread-send eval-thread (list 'expand 1 form cont)))]

           [(list 'swank:swank-macroexpand-all form)
            (let ([eval-thread (get-thread ':repl-thread (current-thread))])
              (thread-send eval-thread (list 'expand 0 form cont)))]

           [(list 'swank:autodoc command _ ...)
            (list 'return `(:return (:ok "([x])") ,cont))]

           [(list 'swank:compile-file-for-emacs fname load?)
            (let ([eval-thread (get-thread ':repl-thread (current-thread))])
              (thread-send eval-thread (list 'compile fname load? cont)))]

           [_ (thread-send
                (current-thread)
                (list 'return `(:return (:ok nil) ,cont)))])))

(define (write-to-connection data out)
  (displayln data)
  (flush-output (current-output-port))
  (display (swank-serialize data) out)
  (flush-output out))
