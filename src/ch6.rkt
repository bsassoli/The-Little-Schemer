#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                       ;
;     CHAPTER 6: SHADOWS                ;
;                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require "ch5.rkt")
(provide (all-defined-out) (all-from-out "ch5.rkt"))


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


(define 1st-sub-exp
  (λ (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (λ (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (λ (aexp)
    (car aexp)))



