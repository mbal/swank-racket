#lang racket

(require racket/tcp
         "getpid.rkt"
         racket/sandbox)

;; transport layer stuff
(define (swank-serialize msg)
  (let ([stringified (~s msg)])
   (string-append (~r (+ (string-length stringified) 1) 
                      #:base 16 #:min-width 6 #:pad-string "0") 
                  stringified "\n")))

(define listener (tcp-listen 4005 5 #t))
(displayln "Running racket-swank on port 4005")

(define (code-complete a) '())

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
  (parameterize ([current-namespace (make-base-empty-namespace)]
                 [current-eval (make-evaluator 'racket/base)])
                (continuously (dispatch-eval parent-thread (thread-receive)))))

(define (dispatch-eval pthread cmd)
  (match cmd
         [(list 'eval string-sexp cont)
          ;; string-sexp is, as the name suggest, a string version of a sexp
          ;; so, first thing, we have to trim and read it.
          (let ([stripped (string-trim string-sexp #:repeat? #t)])
            (when (not (string=? stripped ""))
              (let* ([in (open-input-string string-sexp)]
                     [stx ((current-read-interaction) (object-name in) in)])
                ;; FIXME: we should install an error handler.
                (thread-send pthread (list 'eval-result 
                                           ((current-eval) stx) 
                                           cont)))))]))

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
            `(:return (:ok (:values ,(cadr data))) ,(caddr data)) out))
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
           [(list 'swank:create-repl nil)
            (thread-send
              (current-thread)
              (list 'return `(:return (:ok "racket" "racket") ,cont)))]
           [(list 'swank:listener-eval code) 
            (let ([eval-thread (get-thread thread (current-thread))])
              (thread-send eval-thread (list 'eval code cont)))]
           [(list 'swank:simple-completions pattern _) 
            (thread-send
              (current-thread)
              (list 'return 
                    `(:return (:ok ,(list (code-complete pattern) pattern)) 
                      ,cont)))]
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

;; util
(define-syntax continuously
  (syntax-rules ()
                ((continuously f)
                 (let loop ()
                  f
                  (loop)))))

