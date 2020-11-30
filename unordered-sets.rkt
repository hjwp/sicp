#lang sicp
(newline)

(define (element-of-set? x set)
  ; order n
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(equal? true (element-of-set? 'a '(a b c)))
(equal? true (element-of-set? 'a '(b a c)))
(equal? false (element-of-set? 'd '(b a c)))

(define (adjoin-set x set)
  ; order n because element-of-set is order n
  (if (element-of-set? x set)
      set
      (cons x set)))


(equal? '(a b c) (adjoin-set 'a '(b c)))

(define (make-set l)
  ; order n^2 i think
  (cond ((null? l) '())
        (else (adjoin-set (car l) (make-set (cdr l))))))

(equal? (make-set '(a b b c b)) '(a c b))

(define testset (make-set '(a b c)))
(equal? true (element-of-set? 'a (make-set '(a b c))))
(equal? true (element-of-set? 'b (make-set '(a b c))))
(equal? true (element-of-set? 'c (make-set '(a b c))))
(equal? false (element-of-set? 'd (make-set '(a b c))))


(define (intersection-set set1 set2)
  ; order n^2 bc el-of-set = order n
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(equal? (intersection-set (make-set '(a b c)) (make-set '(b d c)))
        (make-set '(b c)))

(define (union-set set1 set2)
  ; (cond ((null? set2) set1)
  ;       (else (union-set (adjoin-set (car set2) set1) (cdr set2))))) ; this was order n^2 bc adjoin is order n
  ; and this is order n^2 too
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

