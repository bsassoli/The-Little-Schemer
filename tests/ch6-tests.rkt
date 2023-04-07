#lang racket/base


(require rackunit rackunit/text-ui "../src/ch6.rkt")
(provide ch6-tests)

(define ch6-tests
  (test-suite
   "ch6 tests"
(check-true (numbered? 1))
(check-true (numbered? '(3 ∔ (4 † 5))))
(check-false (numbered? '(2 ⨰ sausage)))
(check-equal? (value 13) 13)
(check-equal? (value '(3 ∔ (4 ⨰ 5))) 23)
(check-equal? (value '(3 ∔ 1)) 4)))


