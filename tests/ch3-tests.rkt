#lang racket/base

(require rackunit rackunit/text-ui "../src/ch3.rkt")
(provide ch3-tests)

(define ch3-tests
  (test-suite
   "ch3 tests"
(check-equal? (rember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup))
(check-equal? (firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea))) '(apple plum grape))
(check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
(check-equal? (insertL 'vanilla 'fudge '(ice cream with fudge for dessert)) '(ice cream with vanilla fudge for dessert))
(check-equal? (subst 'mango 'apple '(orange apple banana)) '(orange mango banana))
(check-equal? (subst 'mango 'apple '(orange apple banana lemon)) '(orange mango banana lemon))
(check-equal? (multirember 'apple '(apple orange apple pie apple)) '(orange pie))
(check-equal? (multiinsertR 'quietly 'dog '(jack petted the dog when he saw the dog yawning)) '(jack petted the dog quietly when he saw the dog quietly yawning))
(check-equal? (multiinsertL 'white 'dog '(jack petted the dog when he saw the dog yawning))'(jack petted the white dog when he saw the white dog yawning))
(check-equal? (multisubst 'mango 'apple '(apple orange apple pear mango apple banana apple)) '(mango orange mango pear mango mango banana mango))))
