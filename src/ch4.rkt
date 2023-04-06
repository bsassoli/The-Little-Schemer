#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                   ;
;     CHAPTER 4: NUMBER GAMES       ;
;                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "LittleSchemer_Chapter3.rkt")

(require rackunit)
(provide (all-from-out "LittleSchemer_Chapter3.rkt") (all-defined-out))

(define ∔
  (λ (a b)
    (cond
      [(zero? b) a]
      [else
       (∔ (add1 a) (sub1 b))])))
(check-equal? (∔ 5 7) 12)


(define ∸
  (λ (a b)
    (cond
      [(zero? b) a]
      [else
       (∸ (sub1 b) a)])))
(check-equal?(∸ 7 5) 2)

(define addtup
  (λ (tup)
    (cond
      [(null? tup) 0]
      [else
       (∔ (car tup) (addtup (cdr tup)))])))
(check-equal? (addtup '(2 3 4)) 9)

(define ⨰
  (λ (a b)
    (cond
      [(zero? b) 0]
      [else
       (∔ a (⨰ a (sub1 b)))])))
(check-equal? (⨰ 3 1) 3)
(check-equal? (⨰ 3 5) 15)


(define tup+
  (λ (tup1 tup2)
    (cond
      [(and (null? tup1) (null? tup2)) '()]
      [else
       (cons (∔ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))])))
(check-equal? (tup+ '(2 3 4 5) '(6 5 4 3)) '(8 8 8 8))

(define >
  (λ (a b)
    (cond
      [(zero? a) #f]
      [(zero? b) #t]
      [else (> (sub1 a) (sub1 b))])))
(check-false (> 5 7))
(check-false (> 5 5))
(check-true (> 12 7))

         
(define <
  (λ (a b)
    (cond
      [(and (zero? a) (zero? b)) #f]
      [(zero? a) #t]
      [(zero? b) #f]
      [else (< (sub1 a) (sub1 b))])))
(check-true (< 5 7))
(check-false (< 5 5))
(check-false (< 12 7))

(define =
  (λ (a b)
    (cond
      [(> a b) #f]
      [(< a b) #f]
      (else #t))))
(check-true (= 5 5))
(check-true (= 0 0))
(check-false (= 5 6))
(check-false (= 6 5))

(define †
  (λ (a b)
    (cond
      [(zero? b) 1]
      [else (⨰ a († a (sub1 b)))])))
(check-equal? († 2 0) 1)              
(check-equal? († 3 2) 9)
(check-equal? († 2 3) 8)

(define ÷
  (λ (a b)
    (cond
      [(< a b) 0]
      [else
       (add1 (÷ (∸ a b) b))])))
(check-equal? (÷ 15 3) 5)
(check-equal? (÷ 15 25) 0)

;************************************************************************************************
;
; The Fifth Commandment
;
; When building a value with +, always use 0 for the value of the terminating line,
; for adding 0 does not change the value of an addition.
;
; When building a value with x, always use 1 for the value of the terminating line,
; for multiplying by 1 does not change the value of a multiplication.
;
; When building a value with cons, always consider () for the value of the terminating line.
;
;;************************************************************************************************



(define length
  (λ (lat)
    (cond [(null? lat) 0]
          [else (add1 (length (cdr lat)))])))
(check-equal? (length '(apple orange pie)) 3)
(check-equal? (length '(apple)) 1)

(define pick
  (λ (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))
(check-equal? (pick 3 '(orange apple banana strawberry)) 'banana)

;(define rempick
;  (λ (n lat)
;    (cond
;      [(zero? (sub1 n)) (cdr lat)]
;      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
;(check-equal? (rempick 3 '(orange apple banana strawberry)) '(orange apple strawberry))


(define no-nums
  (λ (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) (no-nums (cdr lat))]
      [else
       (cons (car lat) (no-nums (cdr lat)))])))
(check-equal? (no-nums '(2 3 apple orange 6 8 strawberry)) '(apple orange strawberry))

(define eqan?
  (λ (a1 a2)
    (cond
      [(and (number? a1) (number? a2))
       (cond [(= a1 a2) #t]
             (else #f))]
      (else (cond
              [(eq? a1 a2) #t]
              (else #f))))))
(check-true (eqan? 4 4))
(check-false (eqan? 3 2))
(check-false (eqan? 'apple 2))
(check-true (eqan? 'apple 'apple))
(check-false (eqan? 'apples 'oranges))

(define occur
  (λ (a lat)
    (cond
      [(null? lat) 0]
      [(eqan? (car lat) a) (add1 (occur a (cdr lat)))]
      (else (occur a (cdr lat))))))
(check-equal? (occur 3 '(3 orange 3 apple 1)) 2)
(check-equal? (occur 'mango '(apple orange straberry apple apple)) 0)
(check-equal? (occur 'apple '(apple orange straberry apple apple)) 3)

;(define one?
;  (λ (n)
;    (cond [(zero? (sub1 n)) #t]
;          (else #f))))
;(check-true (one? 1))
;(check-false (one? 4))

(define one?
  (λ (n)
    (= n 1)))
(check-true (one? 1))
(check-false (one? 4))


(define rempick
  (λ (n lat)
    (cond
      [(one? n) (cdr lat)]
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(check-equal? (rempick 3 '(orange apple banana strawberry)) '(orange apple strawberry))