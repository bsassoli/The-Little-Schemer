#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                       ;
;     CHAPTER 7: FRIENDS                ;
;                AND RELATIONS          ;
;                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "LittleSchemer_Chapter1.rkt")
(require "LittleSchemer_Chapter2.rkt")
(require "LittleSchemer_Chapter3.rkt")
(require "LittleSchemer_Chapter4.rkt")
(require "LittleSchemer_Chapter5.rkt")
(require "LittleSchemer_Chapter6.rkt")

(require rackunit)
(provide (all-defined-out))


(define set?
  (λ (lat)
    (cond [(null? lat) #t]
          [(member? (car lat) (cdr lat)) #f]
          [else (set? (cdr lat))])))

(check-false (set? '(apple 4 3 apple 2 1)))
(check-true (set? '(apple 4 3 2 1)))
(check-true (set? '()))
(check-false (set? '(apple 3 pear 4 9 apple 3 4)))

(define makeset
  (λ (lat)
    (cond [(null? lat) '()]
          [(member? (car lat ) (cdr lat)) (makeset (cdr lat))]
          [else (cons (car lat) (makeset (cdr lat)))])))
(check-equal? (makeset '(apple 4 3 apple 2 1)) '(4 3 apple 2 1))

(define subset?
  (λ (s1 s2)
    (cond [(null? s1) #t]
          [(member? (car s1) s2) (subset? (cdr s1) s2)]
          [else #f])))
(check-true (subset? '(apple banana) '(apple pie 2 3 banana)))
(check-false (subset? '(apple 5 banana) '(apple pie 2 3 banana)))

(define eqset?
  (λ (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))
(check-true (eqset? '(apple pie 2 3 banana) '(apple banana pie 2 3)))


(define intersect?
  (λ (s1 s2)
    (cond [(or (null? s1) (null? s2)) #f]
          [(member? (car s1) s2) #t]
          [else (intersect? (cdr s1) s2)])))
(check-true (intersect? '(stewed tomatoes and macaroni)
                        '(macaroni and cheese)))

(define intersect
  (λ (s1 s2)
    (cond [(or (null? s1) (null? s2)) '()]
          [(member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2))]
          [else (intersect (cdr s1) s2)])))
(check-equal? (intersect '(stewed tomatoes and macaroni)
                         '(macaroni and cheese))
              '(and macaroni))


(define union
  (λ (s1 s2)
    (cond [(null? s1) s2]
          [(member? (car s1) s2) (union (cdr s1) s2)]
          [else (cons (car s1) (union (cdr s1) s2))])))
(check-equal? (union '(stewed tomatoes and macaroni casserole)
                     '(macaroni and cheese))
              '(stewed tomatoes casserole macaroni and cheese))

(define intersectall
  (λ (l-set)
    (cond [(null? (cdr l-set)) (car l-set)]
          [else (intersect (car l-set) (intersectall(cdr l-set)))])))
(check-equal? (intersectall '((a b c) (c a d e) (e f g h a b)))
              '(a))
(check-equal? (intersectall '((6 pears and)
                              (3 peaches and 6 peppers)
                              (8 pears and 6 plums)
                              (and 6 prines with some apples)))
              '(6 and))

(define a-pair?
  (λ (x)
    (cond [(atom? x) #f]
          [(null? x) #f]
          [(null? (cdr x)) #f]
          [(null? (cdr (cdr x))) #t]        
          [else #f])))
(check-true (a-pair? '(pear pear)))
(check-true (a-pair? '(3 7)))
(check-true (a-pair? '((2) (pair))))
(check-true (a-pair? '(full (house))))
(check-false (a-pair? '()))
(check-false (a-pair? 'a))
(check-false (a-pair? '(1 2 3)))

(define first
  (λ (y)
    (car y)))

(define second
  (λ (y)
    (car (cdr y))))

(define build
  (λ (s1 s2)
    (cons s1 (cons s2 '()))))
(check-true (a-pair? (build 'doggo 'love)))

(define fun?
  (λ (rel)
    (set? (firsts rel))))
(check-false (fun? '((4 3) (4 2) (7 6) (6 2) (3 4))))
(check-true (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(check-false (fun? '((d 4) (b 0) (b 9) (e 5) (g 4))))


(define revpair
  (λ (pair)
    (build (second pair) (first pair))))

(define revrel
  (λ (rel)
    (cond [(null? rel) '()]
          [else (cons (revpair (car rel)) (revrel (cdr rel)))])))
(check-equal? (revrel '((8 a) (pumpkin pie) (got sick)))
              '((a 8) (pie pumpkin) (sick got)))

(define seconds
  (λ (l)
    (cond [(null? l) '()]
     [else (cons (second (car l)) (seconds (cdr l)))])))

(define fullfun?
  (λ (fun)
    (set? (seconds fun))))

(check-false (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(check-true (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))))
(check-false (fullfun? '((grape raisin)
                         (plum prune)
                         (stewed prune))))
(check-true (fullfun? '((grape raisin)
                        (plum prune)
                        (stewed grape))))


(define one-to-one?
  (λ (fun)
    (fun? (revrel fun))))
(check-true (one-to-one? '((chocolate chip) (doughy cookie))))