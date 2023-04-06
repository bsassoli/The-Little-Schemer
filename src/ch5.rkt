#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                       ;
;     CHAPTER 5: OH MY GAWD*:           ;
;                IT'S FULL OF STARS     ;
;                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require "LittleSchemer_Chapter4.rkt")

(require rackunit)
(provide (all-from-out) (all-defined-out))

(define rember*
  (λ (a l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(eq? (car l) a) (rember* a (cdr  l))]
                 [else (cons (car l) (rember* a (cdr  l)))])]
          [else (cons (rember* a (car l)) (rember* a (cdr l)))])))
(check-equal? (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
              '((coffee) ((tea)) (and (hick))))

(define insertR*
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eqan? (car l) old) (cons old (cons new (insertR* new old (cdr l))))]
         [else (cons (car l) (insertR* new old (cdr l)))])]
      [else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))])))
(check-equal? (insertR* 'roast 'chuck '((how much (wood))
                                        could
                                        ((a (wood) chuck))
                                        (((chuck)))
                                        (if (a) ((wood chuck)))
                                        could chuck wood))
              '((how much (wood))
                could
                ((a (wood) chuck roast))
                (((chuck roast)))
                (if (a) ((wood chuck roast)))
                could chuck roast wood))


;;**********************************************************
;; The First Commandment (final version)
;;
;; When recurring on a list of atoms, lat, ask two questions
;; about it: (null? lat) and else.
;; When recurring on a number, n, ask two questions about
;; it: (zero? n) and else.
;; When recurring on a list of S-expressions, l, ask three
;; questions about it: (null? l, (atom? (car l)), and else.
;;**********************************************************

;;**********************************************************
;; The Fourth Commandment (final version)
;;
;; Always change at least one argument while recurring.
;; When recurring on a list of atoms, lat, use (cdr lat).
;; When recurring on a number, n, use (sub1 n). And when
;; recurring on a list of S-expressions, l, use (car l) and
;; (cdr l) if neither (null? l) nor (atom? (car l)) are true.
;;
;; It must be changed to be closer to termination. The
;; changing argument must be tested in the termination condition:
;;
;; when using cdr, test termination with null? and
;; when using sub1, test termination with zero?.
;;**********************************************************

(define occur*
  (λ (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (cond [(eqan? a (car l)) (add1 (occur* a (cdr l)))]
             [else (occur* a (cdr l))])]
      [else
       (∔ (occur* a (car l)) (occur* a (cdr l)))])))
(check-equal? (occur* 'banana '((banana)
                                (split ((((banana ice)))
                                        (cream (banana))
                                        sherbet))
                                (banana)
                                (bread)
                                (banana brandy)))
              5)

(define subst*
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond [(eqan? (car l) old) (cons new (subst* new old (cdr l)))]
             [else (cons (car l) (subst* new old (cdr l)))])]
      [else (cons (subst* new old (car l)) (subst* new old (cdr l)))])))

(check-equal? (subst* 'orange 'banana '((banana)
                                        (split ((((banana ice)))
                                                (cream (banana))
                                                sherbet))
                                        (banana)
                                        (bread)
                                        (banana brandy)))
              '((orange)
                (split ((((orange ice)))
                        (cream (orange))
                        sherbet))
                (orange)
                (bread)
                (orange brandy)))


(define insertL*
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond [(eqan? (car l) old) (cons new (cons old (insertL* new old (cdr l))))]
             [else (cons (car l) (insertL* new old (cdr l)))])]
      [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))])))
              
(check-equal? (insertL* 'pecker 'chuck '((how much (wood))
                                         could
                                         ((a (wood) chuck))
                                         (((chuck)))
                                         (if (a) ((wood chuck)))
                                         could chuck wood))
              '((how much (wood))
                could
                ((a (wood) pecker chuck))
                (((pecker chuck)))
                (if (a) ((wood pecker chuck)))
                could pecker chuck wood))

(define member*
  (λ (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (cond [(eqan? (car l) a) #t]
             (else (member* a (cdr l))))]
      [else (or (member* a (car l)) (member* a (cdr l)))])))
(check-true (member* 'chips '((potato) (chips ((with) fish) (chips)))))

(define leftmost
  (λ (l)
    (cond
      [(atom? (car l)) (car l)]
      [else
       (leftmost (car l))])))
(check-equal? (leftmost '((potato (chips ((with) fish) (chips)))))
              'potato)
(check-equal? (leftmost '(((hot) (tuna (and))) cheese))
              'hot)

(define eqlist?
  (λ (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t] ; both lists are empty -> #t
      [(null? l1) #f] ; first list is empty -> #f (because we know the second one is not empty so they're not the same)
      [(null? l2) #f] ; second list is empty -> #f (becasue we know the first one is not empty so they're not the same)     
      [(and (atom? (car  l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))] ; both lists start with atoms: check if the first atoms are equal and recur
      [(atom? (car l1)) #f] ; l1 starts with an atom? #f (because we know l2 doesn't)
      [(atom? (car l2)) #f]; l2 starts with an atom? #f (because we know l1 doesn't)
      [else
       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]))); recur on both car and cdrs of both lists         
(check-true (eqlist? '(strawberry ice cream)
                     '(strawberry ice cream)))
(check-false (eqlist? '(strawberry ice cream)
                      '(strawberry cream ice)))
(check-false (eqlist? '(banana ((split)))
                      '((banana (split)))))
(check-false (eqlist? '(beef ((sausage)) (and (soda)))
                      '(beef ((salami)) (and (soda)))))
(check-true (eqlist? '(beef ((sausage)) (and (soda)))
                     '(beef ((sausage)) (and (soda)))))


(define equal?
  (λ (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
      [else (eqlist? s1 s2)])))   
(check-true (equal? '() '()))
(check-false (equal? 1 2))
(check-true (equal? 3 3))
(check-true (equal? '(a b (c d)) '(a b (c d))))
(check-true (equal? '(1 2 (3 c) ((words)) (more (((words)) a)))
                    '(1 2 (3 c) ((words)) (more (((words)) a)))))
(check-false (equal? '(1 2 (3 c) ((words)) (more (((words)) a)))
                     '(1 2 (3 c) ((words)) ((more) (((words)) a)))))


;;**********************************************************
;; The Sixth Commandment
;;
;; Simplify only after the function is correct.
;;
;;**********************************************************


