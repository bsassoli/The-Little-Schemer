#lang racket/base

(require rackunit rackunit/text-ui "../src/ch2.rkt")
(provide ch2-tests)

(define ch2-tests
  (test-suite
   "ch2 tests"
(check-true (lat? '(bacon egg)))
(check-false (lat? '((bacon ham) egg)))
(check-true (member? 'a '(b c a d)))
(check-false (member? 'a '(b c d)))
(check-true (member? 'a '(a)))
(check-false (member? 'a '()))))

