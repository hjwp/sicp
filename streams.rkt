#lang sicp
(newline)
(display "--------------------------------------------")
(newline)

; -- some stuff from earlier chapters --
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

; ---------------------------
; TODO: further investigation required

; this won't work bc delay has to be a special form I think???
; (define (delay exp) (lambda () exp))
; (define (delay exp) (memo-proc (lambda () exp)))
; 
; ya bc otherwise exp would be evaluated immediately or sumfink

; ---------------------------

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
        ; takewhile  !=  filter.  once pred is false we just bail
        the-empty-stream))))

(display "this should show 1-99 ")
(takewhile (lambda (x) (< x 100)) integers)

(newline)


;(define (fibgen a b)
; (cons-stream a (fibgen b (+ a b))))

(define (fibs-starting-from n-2 n-1)
  (let ((n (+ n-2 n-1)))
    (cons-stream n-2 (fibs-starting-from n-1 n))))
(define fibs (fibs-starting-from 0 1))

(display "this should print fibs up to 34 55 89: ")
(takewhile (lambda (x) (< x 100)) fibs)


;; -- sieve of Eratosthenes --

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define (take-n stream n) 
  (cond
  ((null? (stream-car stream)) the-empty-stream)
  ((= n 0) the-empty-stream)
  (else
    (cons (stream-car stream) (take-n (stream-cdr stream) (- n 1))))))

(display "this should print the first 10 primes: ")
(take-n primes 10)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; mindblown, this feels like it should be just multiples of 2 but its powers of 2
(define double (cons-stream 1 (scale-stream double 2)))
(display "this should print powers of 2 (!): ")
(take-n double 10)

(display "this should print multiples of 2: ")
(define multiples-of-2 (scale-stream integers 2))
(take-n multiples-of-2 10)

; ok that makes sense

; --- moar infinite streaaaaaams ----
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers2 (cons-stream 1 (add-streams ones integers2)))

; Exercise 3.53.  Without running the program, describe the elements of the
; stream defined by
(define s (cons-stream 1 (add-streams s s)))
(display "this should print 1, 2, 4, 8, 16...: ")
(take-n s 10)

; Exercise 3.54.  Define a procedure mul-streams, analogous to add-streams,
; that produces the elementwise product of its two input streams.
; Use this together with the stream of integers to complete 
; the following definition of the stream whose nth element (counting from 0)
; is n + 1 factorial:

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(display "factorials;")
(newline)
(define factorials (cons-stream 1 (mul-streams factorials integers)))
(display "this should print 1, 2, 6, 24, 120,... ")
(take-n factorials 10)


; -- streams as iterations --

(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
   guesses)


(display "approximations of root-2: ")
(take-n (sqrt-stream 2) 7)

; Exercise 3.55.  Define a procedure partial-sums that takes as argument a stream S and returns the stream whose elements are S0, S0 + S1, S0 + S1 + S2, .... For example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15, .... 
(define (partial-sums s)
  (cons-stream
    (stream-car s)
    (stream-map + s (partial-sums s))))

; TODO: that's not working yet

(display "partial sums on integers, shld be 1 3 6 10 15:")
(take-n (partial-sums integers) 10)


(define (pi-summands n)
  (cons-stream 
    (/ 1.0 n)
    (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))


(display "approximations of pi: ")
(take-n pi-stream 10)


; -- the end --
(newline)
(display "OK")
(newline)
