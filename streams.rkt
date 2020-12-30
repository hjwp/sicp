#lang sicp
(newline)
(display "--------------------------------------------")
(newline)

; some stuff from earlier chapters:
(define (square x) (* x x))

; primes
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


; normal list map/filtering
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval lo hi)
  (if 
    (> lo hi) '()
    (cons lo (enumerate-interval (+ lo 1) hi))))

(display "interval 1-20 ")
(enumerate-interval 1 20)

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

(display "primes 1-1oo ")
(filter prime? (enumerate-interval 1 100))

(display "sum-primes 1-1oo: ")
(sum-primes 1 100)
