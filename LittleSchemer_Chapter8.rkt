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
