#lang sicp
(newline)

(define (element-of-set? x set)
  ; still order n
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(equal? true (element-of-set? 'a '(a b c)))
(equal? true (element-of-set? 'a '(b a c)))
(equal? false (element-of-set? 'd '(b a c)))

(define (adjoin-set x set)
  ; constant time
  (cons x set))

(equal? '(a b c) (adjoin-set 'a '(b c)))

(define (make-set l) l)  ; constant time

(equal? (make-set '(a b b c b)) '(a b b c b))

(define testset (make-set '(a b c)))
(equal? true (element-of-set? 'a (make-set '(a b c))))
(equal? true (element-of-set? 'b (make-set '(a b c))))
(equal? true (element-of-set? 'c (make-set '(a b c))))
(equal? false (element-of-set? 'd (make-set '(a b c))))


(define (intersection-set set1 set2)
  ; still n^2
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(equal? (intersection-set (make-set '(a b c)) (make-set '(b d c)))
        (make-set '(b c)))

(define (union-set set1 set2)
  ; order n
  (make-set (append set1 set2)))


(define testunionset (union-set (make-set '(a b c)) (make-set '(c d e))))
(equal? true (element-of-set? 'a testunionset))
(equal? true (element-of-set? 'b testunionset))
(equal? true (element-of-set? 'c testunionset))
(equal? true (element-of-set? 'd testunionset))
(equal? true (element-of-set? 'e testunionset))

testunionset
(equal? testunionset '(a b c d e))

(newline)


; 2.60 answer: it's the same for element-of, same for intersection, and better for adjoin + union.
; but presumably it would start to suck for large sets?

