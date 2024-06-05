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
  (if (null? (cdr l)) l  ; ie l has just 1 item
    (append
      (reverse1 (cdr l)) ; reverse the cdr
      (list (car l)))))  ; a list containing the car

(check-equal? (reverse1 (list 1 4 9 16 25)) '(25 16 9 4 1))


(define (last-item l)
  (if (null? (cdr l)) (car l)
    (last-item (cdr l))))

(define (all-but-last l)
  (if (null? (cddr l)) (list (car l))
    (cons (car l) (all-but-last (cdr l)))))

(define (reverse2 l) 
  (if (null? (cdr l)) l
    (cons (last-item l) (reverse (all-but-last l)))))

(define (reverse l) 
  (define (reverse-iter l acc)
    (if (null? l) acc
      (reverse-iter (cdr l)
                    (cons (car l) acc))))

  (reverse-iter l '()))

(check-equal? (reverse '(1)) '(1))
(check-equal? (reverse '(1 2)) '(2 1))

(display "here") (newline)
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
  (begin
    (display "deep-reversing ")
    (display tree)
    (newline)
    (cond
      ((null? tree) (begin (display "null") (newline) tree))
      ((not (pair? tree)) (begin (display "notpair") (newline) tree))
      (else
        (reverse (map deep-reverse tree))))))

(define nestey 
  (list (list 1 2) (list 3 4)))

(null? (cdr '(1)))
(check-equal? (reverse nestey) '((3 4) (1 2)))

(check-equal? (deep-reverse '()) '())
(check-equal? (deep-reverse '(1 2 3)) '(3 2 1))
(check-equal? (deep-reverse '(3 4)) '(4 3))

(check-equal? (deep-reverse nestey) '((4 3) (2 1)))

(cons '() '(1 2))

;; first fringe solution involving an iterative helper and (append)
;; but does the "append" make it O(n²)?
(define (fringe1 tree)
  (define (fringe-iter tree acc)
    (cond
      ((null? tree) acc)
      ((not (pair? tree)) (cons tree acc))
      (else
        (append
          (fringe-iter (car tree) acc)
          (fringe-iter (cdr tree) '())))))

  (fringe-iter tree '()))

;; second fringe solution, recursive with (append) and we always return a list
;; also O(n2) tho??

(define (fringe2 tree)
  (cond
    ((null? tree) tree)
    ((not (pair? tree)) (list tree))
    (else
      (append
        (fringe (car tree))
        (fringe (cdr tree))))))

;; (define (first-leaf tree)
;;   (cond
;;     ((null? tree) tree)
;;     ((not (pair? tree)) tree)
;;     (first-leaf (car tree))))

;; an attempt to define a more efficient recursive solution,
;; without the worrying  (append)

(define (fringe tree)
  (define (fringe-iter t acc)
    ((null? tree) acc)
    ((not (pair? tree)) (cons tree acc))
    (else
      ;; apparently this isn't ok??
      (fringe-iter
        (car tree)
        (fringe-iter (cdr tree) acc))))
  (fringe-iter tree '()))


(check-equal? (fringe '(1 2 3)) '(1 2 3))
(check-equal? (fringe '((1 2) (3 4 5 (6 7 (8))) 9)) '(1 2 3 4 5 6 7 8 9))

;; (check-equal? (fringe '(((1 2) (3 4)) ((1 2) (3 4)))) '(1 2 3 4 1 2 3 4))
