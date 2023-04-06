#lang racket/base
(require rackunit rackunit/text-ui "../src/ch1.rkt")
(provide ch1-tests)

(define (sexp? exp) (or (atom? exp) (list? exp)))

; TESTS BEGIN HERE
(define ch1-tests
  (test-suite
   "ch1 tests"
   (test-case "test atom?"
              (check-false (atom? '()) "null list is atom")
              (check-true (atom? 'atom) "simple string is atom")
              (check-true (atom? 'turkey) "another simple string is atom")
              (check-true (atom? 1492) "four digit int is atom")
              (check-true (atom? 'u) "one char is atom")
              (check-true (atom? '*abc$)) "string with non alphabetic chars is atom")
   (test-case "test list?"
              (check-true (list? '()) "empty list is a list")
              (check-true (list? '(atom)) "one item list is a list")
              (check-true (list? '(atom turkey or)) "three item list is a list")
              (check-true (list? '((atom turkey) or)) "nested list is a list"))

   (test-case "test sexp?"
              (check-true (sexp? 'xyz) "three char string is a sexp")
              (check-true (sexp? '(x y z)) "list of three is a sexp")
              (check-true (sexp? '((x y) z)) "nested list is a sexp"))))
              




