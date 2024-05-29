#lang sicp
(#%require (only racket require))
(require rackunit)

(define eg1 (list 1 3 (list 5 7) 9))
(define eg1b '(1 3 (5 7) 9))

; (check-equal? (cdr (car (cdr (cdr eg1)))) 7) 
(check-equal? (car (cdr (car (cdr (cdr eg1))))) 7) 
(check-equal? (car (cdr (car (cdr (cdr eg1b))))) 7) 

(define eg2 '((7)))
(check-equal? (car (car eg2)) 7) 

(define eg3 '(1 (2 (3 (4 (5 (6 7)))))))

(check-equal? (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr eg3)))))))))))) 7)


(define (list-ref items n)
  (if (= n 0) (car items)
    (list-ref (cdr items) (- n 1))))

(check-equal? (list-ref (list 1 2 3) 2) 3)

(define (length items)
  (if (null? items) 
    0
    (+ 1 (length (cdr items)))))

(check-equal? (length (list 1 2 3)) 3)


(define (append l1 l2)
  (if (null? l1)
    l2
    (cons
      (car l1)
      (append (cdr l1) l2))))

(check-equal? (append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))


(define (reverse1 l) 
  (if (null? (cdr l)) l
    (append
      (reverse1 (cdr l))
      (list (car l)))))

(check-equal? (reverse1 (list 1 4 9 16 25)) '(25 16 9 4 1))


(define (last-item l)
  (if (null? (cdr l)) (car l)
    (last-item (cdr l))))

(define (all-but-last l)
  (if (null? (cddr l)) (list (car l))
    (cons (car l) (all-but-last (cdr l)))))

(define (reverse l) 
  (if (null? (cdr l)) l
    (cons (last-item l) (reverse (all-but-last l)))))

(check-equal? (reverse '(1)) '(1))
(check-equal? (reverse '(1 2)) '(2 1))

(check-equal? (reverse (list 1 4 9 16 25)) '(25 16 9 4 1))


; map buitlin
(define (square x) (* x x))
(map square '(1 2 3))


(define x (list 1 2 3))
(define y (list 4 5 6))

(check-equal? (append x y) (list 1 2 3 4 5 6))
(check-equal? (cons x y) '((1 2 3) 4 5 6))
(check-equal? (list x y) '((1 2 3) (4 5 6)))


(define (deep-reverse tree)
  (cond
    ((null? tree) tree)
    ((not (pair? tree)) tree)
    (cons
      (deep-reverse (cdr tree))
      (deep-reverse (car tree)))))

(define nestey 
  (list (list 1 2) (list 3 4)))

(null? (cdr '(1)))
(check-equal? (reverse nestey) '((3 4) (1 2)))

(check-equal? (deep-reverse nestey) '((4 3) (2 1)))
