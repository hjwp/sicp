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

; dont do this its slow
;(car (cdr (filter prime?
;                 (enumerate-interval 10000 1000000))))

; --------- STREAM STUFF BEGINS IN EARNEST ---------

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map-old proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map-old proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (delay exp) (memo-proc (lambda () exp)))

(display "this should be 10009: ")
(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))

; Exercise 3.50.  Complete the following definition,
; which generalizes stream-map to allow procedures that take multiple arguments,
; analogous to map in section 2.2.3, footnote 12.

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define addedstreams
  (stream-map
    (lambda (x y) (+ x y))
    (stream-enumerate-interval 100 100000)
    (stream-enumerate-interval 300 300000)
    ))
(display "this should be 402: ")
(stream-car (stream-cdr addedstreams))

(define singleargsstillworks
  (stream-map
    (lambda (x) (* x 2))
    (stream-enumerate-interval 100 100000)))
(display "this should be 202: ")
(stream-car (stream-cdr singleargsstillworks))

; Exercise 3.51.  In order to take a closer look at delayed evaluation,
; we will use the following procedure,
; which simply returns its argument after printing it:
(define (show x)
  (display-line x)
  x)
; What does the interpreter print in response to evaluating each expression in the following sequence?

(display "this should show nothing: ")
(define x (stream-map show (stream-enumerate-interval 0 10)))
(display " <- interesting why a zero here i wonder...")
(display " because the first car in the stream is not delayed?")
(newline)

(display "this should show 1-5, then return 5")
(stream-ref x 5)

(display "this should show 6+7. and then return 7")
(stream-ref x 7)


;=== 3.5.2  Infinite Streams  ===
(newline)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
; 🤯
(define integers (integers-starting-from 1))

(display "this should show 3: ")
(stream-car (stream-cdr (stream-cdr integers)))

; one i made up for myself ok
(define (takewhile pred stream)
  (if (null? (stream-car stream)) the-empty-stream
    (let ((first (stream-car stream)))
      (if (pred first) (cons first (takewhile pred (stream-cdr stream)))
        the-empty-stream))))

(display "this should show 1-99 ")
(takewhile (lambda (x) (< x 100)) integers)



(newline)
(display "OK")
(newline)

