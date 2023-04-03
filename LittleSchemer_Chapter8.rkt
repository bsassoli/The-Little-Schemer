#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                       ;
;     CHAPTER 8: LAMBDA THE ULTIMATE    ;
;                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "LittleSchemer_Chapter1.rkt")
(require "LittleSchemer_Chapter2.rkt")
(require "LittleSchemer_Chapter3.rkt")
(require "LittleSchemer_Chapter4.rkt")
(require "LittleSchemer_Chapter5.rkt")
(require "LittleSchemer_Chapter6.rkt")
(require "LittleSchemer_Chapter7.rkt")

(require rackunit)
(provide (all-defined-out))


;(define rember-f
;  (λ (test? a l)
;    (cond [(null? l) '()]
;          [(test? (car l) a) (cdr l)]
;          [else (cons (car l)  (rember-f test? a (cdr l)))])))
;(check-equal? (rember-f = 5 '(6 2 5 3))
;              '(6 2 3))
;(check-equal? (rember-f eq? 'jelly '(jelly beans are good))
;              '(beans are good))
; REWRITE CURRIED


(define rember-f
  (λ (test?)
    (λ (a l)
      (cond [(null? l) '()]
            [(test? (car l) a) (cdr l)]
            [else (cons (car l)  ((rember-f test?) a (cdr l)))]))))
(check-equal? ((rember-f =) 5 '(6 2 5 3))
              '(6 2 3))
(check-equal? ((rember-f eq?) 'jelly '(jelly beans are good))
              '(beans are good))

; CURRYING
; a function that, when passed an argument a, returns the function
; (lambda (x)
;    (eq? x a)))
; where a is just that argument.

; (lambda (a)
;  (lambda (x)
;    (eq? x a)))

(define eq?-c
  (λ (a)
    (λ (x)
      (eq? a x))))
(check-false ((eq?-c 'salad) 'tuna))
(check-true ((eq?-c 'salad) 'salad))

(define eq?-salad
  (eq?-c 'salad))
(check-true (eq?-salad 'salad))
(check-false (eq?-salad 'tuna))

(define insertL-f
  (λ (test?)
    (λ (old new l)
      (cond [(null? l) '()]
            [(test? (car l) old) (cons new (cons old (cdr l)))]
            [else (cons (car l) ((insertL-f test?) (old new (cdr l))))]))))
(check-equal? ((insertL-f eq?) 'tuna 'potatoes '(tuna salad)) '(potatoes tuna salad))

(define insertR-f
  (λ (test?)
    (λ (old new l)
      (cond [(null? l) '()]
            [(test? (car l) old) (cons old (cons new (cdr l)))]
            [else (cons (car l) ((insertR-f test?) (old new (cdr l))))]))))
(check-equal? ((insertR-f eq?) 'tuna 'potatoes '(tuna salad)) '(tuna potatoes salad))

(define seqL
  (λ (old new l)
    (cons new (cons old (cdr l)))))
(define seqR
  (λ (old new l)
    (cons old (cons new (cdr l)))))

(define insert-g
  (λ (seq)
    (λ (old new l)
      (cond [(null? l) '()]
            [(eq? (car l) old) (seq old new l)]
            [else (cons (car l) ((insert-g seq) (old new (cdr l))))]))))
(check-equal?
 ((insertL-f eq?) 'tuna 'potatoes '(tuna salad))
 ((insert-g seqL) 'tuna 'potatoes '(tuna salad))) 
(check-equal?
 ((insertR-f eq?) 'tuna 'potatoes '(tuna salad))
 ((insert-g seqR) 'tuna 'potatoes '(tuna salad))) 


(define atom-to-function
  (λ (x)
    (cond
      [(eq? x '+) ∔]
      [(eq? x 'x) ⨰]
      [else †])))      

(define value-f
  (λ (nexp)
    (cond [(atom? nexp) nexp]
          [else ((atom-to-function (operator nexp)) (value-f (1st-sub-exp nexp)) (value-f (2nd-sub-exp nexp)))])))

(check-equal? (value-f '(+ 7 (x 2 5))) 17)

(define multirember-f
  (λ (test?)
    (λ (a l)
      (cond [(null? l) '()]
            [(test? a (car l)) ((multirember-f test?) a (cdr l))]
            [else (cons (car l) ((multirember-f test?) a (cdr l)))]))))
(check-equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

(define multiremberT
  (λ (test? lat)
    (cond [(null? lat) '()]
          [(test? (car lat)) (multiremberT test? (cdr lat))]
          [else (cons (car lat) (multiremberT test? (cdr lat)))])))
(define eq?-tuna
  (λ (x)
    (eq? x 'tuna)))
(check-equal? (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))


(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col (quote ()) (quote ())))
      ((eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (λ (x y)
    (null? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
              (cons ('tuna) seen))))

(check-true (multirember&co 'tuna '() a-friend))
(check-false (multirember&co 'tuna '(tuna) a-friend))
(check-false (multirember&co 'tuna '(and tuna) a-friend))

(check-equal? (multirember&co 'tuna '(strawberries tuna and tuna swordfish) (lambda (x y) (length x)))
              3)
(check-equal? (multirember&co 'tuna '(strawberries tuna and tuna swordfish) (lambda (x y) (length y)))
              2)

;;**********************************************************
;; The Tenth Commandment
;;
;; Build functions to collect more than one value at a time.
;;**********************************************************


; Hint: multiinsertLR inserts new to the left of oldL and to the right of oldR in lat if oldL are oldR are different.
(define multiinsertLR
  (λ (new oldL oldR lat)
    (cond [(null? lat) '()]
          [(eq? oldL (car lat))
           (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))]
          [(eq? oldR (car lat))
           (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))]
          [else
           (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))])))
(check-equal? (multiinsertLR 'tuna 'salad 'grilled '(appetizer of salad and grilled))
              '(appetizer of tuna salad and grilled tuna))
(check-equal? (multiinsertLR 'tuna 'salad 'salad '(appetizer of salad and grilled fish))
              '(appetizer of tuna salad and grilled fish))

;(define multiinsertLD&co ())


(define even?
  (λ (n)
    (= (modulo n 2) 0)))


(define evens-only*
  (λ (l)
    (cond [(null? l) '()]
          [(atom?  (car l))
           (cond [(even? (car l)) (cons (car l) (evens-only* (cdr l)))]
                 [else (evens-only* (cdr l))])]
          [else (cons (evens-only* (car l)) (evens-only* (cdr l)))])))
(check-equal? (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
              '((2 8) 10 (() 6) 2))