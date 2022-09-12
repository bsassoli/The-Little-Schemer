#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                       ;
;     CHAPTER 5: OH MY GAWD*:           ;
;                IT'S FULL OF STARS     ;
;                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "LittleSchemer_Chapter1.rkt")
(require "LittleSchemer_Chapter2.rkt")
(require "LittleSchemer_Chapter3.rkt")

(require rackunit)
(provide (all-defined-out))

(define rember*
  (λ (a l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(eq? a (car l)) (rember* a (cdr l))]
                 [else (cons (car a) (rember* a (cdr l)))])
           (else (cons (rember* a (car l)) (rember* a (cdr l))))])))