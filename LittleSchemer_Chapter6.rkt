#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                       ;
;     CHAPTER 6: SHADOWS                ;
;                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "LittleSchemer_Chapter1.rkt")
(require "LittleSchemer_Chapter2.rkt")
(require "LittleSchemer_Chapter3.rkt")
(require "LittleSchemer_Chapter4.rkt")
(require "LittleSchemer_Chapter5.rkt")

(require rackunit)
(provide (all-defined-out))


(define numbered?
  (λ (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [(eq? (car (cdr aexp)) '∔) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
      [(eq? (car (cdr aexp)) '⨰) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
      [(eq? (car (cdr aexp)) '†) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))])))

(define value
  (λ (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (car (cdr nexp)) '∔) (∔ (value (car nexp)) (value (car (cdr (cdr nexp)))))]
      [(eq? (car (cdr nexp)) '⨰) (⨰ (value (car nexp)) (value (car (cdr (cdr nexp)))))]
      [(eq? (car (cdr nexp)) '†) († (value (car nexp)) (value (car (cdr (cdr nexp)))))])))

(check-true (numbered? 1))
(check-true (numbered? '(3 ∔ (4 † 5))))
(check-false (numbered? '(2 ⨰ sausage)))
(check-equal? (value 13) 13)

(check-equal? (value '(3 ∔ (4 ⨰ 5))) 23)
(check-equal? (value '(3 ∔ 1)) 4)

(define 1st-sub-exp
  (λ (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (λ (aexp)
    (cdr (cdr aexp))))

(define operator
  (λ (aexp)
    (car aexp)))

(1st-sub-exp '(∔ 3 († 4  5)))
(2nd-sub-exp '(∔ 3 († 4  5)))
(operator '(∔ 3 († 4  5)))


