#lang racket

(require rackunit
         "../util.rkt")

(test-equal? "test message serialization"                
             (swank-serialize '(:return (:ok nil) 1))
             "000016(:return (:ok nil) 1)\n")

(test-equal? "test message serialization"                
             (swank-serialize '(:return (:deeply (:nested (:list 1) 2) 3) 4))
             "00002e(:return (:deeply (:nested (:list 1) 2) 3) 4)\n")
