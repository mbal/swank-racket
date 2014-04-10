#lang racket

(require 
  racket/tcp
  "control.rkt"
  "util.rkt")

(define listener (tcp-listen 4005 5 #t))
(displayln "Running racket-swank on port 4005")

(define (start-server)
  (displayln "Waiting for the client to connect")
  (define-values (i o) (tcp-accept listener))
  (displayln "Client connection accepted!")

  (define control-thread
    (thread (lambda () (control-loop i o))))

  ;; the main thread reads from the connection and puts data in the
  ;; control thread's mailbox.
  (continuously 
    (begin
      (read i)
      (thread-send control-thread (read i))))

  (thread-wait control-thread)
  (display "server ended"))

(start-server)
