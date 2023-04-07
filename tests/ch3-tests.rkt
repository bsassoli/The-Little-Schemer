#lang racket/base

(require rackunit rackunit/text-ui "../src/ch3.rkt")
(provide ch3-tests)



(define ch3-tests
  (test-suite
   "ch3 tests"
   (test-case "check rember?"
              (check-equal? (rember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup) "remove one atom from list where that atom is repeated")
              (check-equal? (rember 'apple '(coffee tea banana)) '(coffee tea banana) "rember on empy list")
              (check-equal? (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce) "remove one atom from list where that atom is repeated"))
   (test-case "check firsts"
              (check-equal? (firsts '((apple))) '(apple) "check firsts with single list of one item")
              (check-equal? (firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea))) '(apple plum grape) "checks first on list of multiple lists of equal length")
              (check-equal? (firsts '()) '() "check firsts on empty list")
              (check-equal? (firsts '((apple peach) (pear) (grape raisin pea))) '(apple pear grape) "check firsts with lists of different lenghts")
              (check-equal? (firsts '((apple peach) () (grape raisin pea))) '(apple grape) "check firsts with lists containing empty list"))
   (test-case "check-seconds"
              (check-equal? (seconds '((apple peach pumpkin) (plum pear cherry) (grape raisin pea))) '(peach pear raisin) "checks seconds on list of multiple lists of equal length") 
              (check-equal? (seconds '((apple peach) (pear) (grape raisin pea))) '(peach raisin) "check seconds with lists of different lenghts")
              (check-equal? (seconds '()) '() "check seconds with empty list")
              (check-equal? (seconds '((apple peach) () (grape raisin pea))) '(peach raisin)))
   (test-case "check insertR"
              (check-equal? (insertR 'topping 'fudge '(fudge)) '(fudge topping) "insertR on list of one item")
              (check-equal? (insertR 'topping 'fudge '(ice cream)) '(ice cream)  "insertR on list where old isn't present")
              (check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert) "insertR on list with multiple items")
              (check-equal? (insertR 'topping 'fudge '()) '() "insertR on empty list"))
   (test-case "check insertL"
              (check-equal? (insertL 'vanilla 'fudge '(ice cream with fudge for dessert)) '(ice cream with vanilla fudge for dessert) "susbt with long list")
              (check-equal? (insertL 'vanilla 'ice '(ice)) '(vanilla ice) "subst with one-item list"))
   (test-case "subst"
                (check-equal? (subst 'mango 'apple '(orange apple banana)) '(orange mango banana))
                (check-equal? (subst 'mango 'apple '(orange banana lemon)) '(orange banana lemon)))
   (check-equal? (multirember 'apple '(apple orange apple pie apple)) '(orange pie))
   (check-equal? (multiinsertR 'quietly 'dog '(jack petted the dog when he saw the dog yawning)) '(jack petted the dog quietly when he saw the dog quietly yawning))
   (check-equal? (multiinsertL 'white 'dog '(jack petted the dog when he saw the dog yawning))'(jack petted the white dog when he saw the white dog yawning))
   (check-equal? (multisubst 'mango 'apple '(apple orange apple pear mango apple banana apple)) '(mango orange mango pear mango mango banana mango))))
(run-tests ch3-tests)