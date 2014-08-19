(module repl racket/base
  (provide repl-anchor *1 *2 *3 *e update-vars!)
  (define-namespace-anchor repl-anchor)
  
  (define *1 #f) (define *2 #f) (define *3 #f) (define *e #f) 
  
  (define (update-vars! n1 n2 n3 ne)
    ;; Updates the variables. Note that the order *is not* important:
    ;; n1 n2 n3 and ne have already been evaluated and they have the
    ;; correct value.
    ;; (update-vars! *2 *1 ...) would cause problems if the arguments
    ;; are evaluated when needed (i.e. *2 wouldn't be updated)
    (set! *1 n1)
    (set! *2 n2)
    (set! *3 n3)
    (set! *e ne)))
