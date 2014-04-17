#lang racket
(require rackunit
         "../util.rkt")

(test-equal? "fix-truth-values on empty"
             (schemify-truth-values '())
             '())

(test-equal? "fix-truth-values without truth"
             (schemify-truth-values '(1 (2 3 (4))))
             '(1 (2 3 (4))))

(test-equal? "fix-truth-values without truth"
             (schemify-truth-values '(symb (2 anothersymbol (4))))
             '(symb (2 anothersymbol (4))))

(test-equal? "fix-truth-values only truth"
             (schemify-truth-values '(#f (#t #f #t)))
             '(#f (#t #f #t)))

(test-equal? "fix-truth-values only truth"
             (schemify-truth-values '(nil (t nil t)))
             '(#f (#t #f #t)))
