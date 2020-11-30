#lang sicp
(newline)

; rand is not built-in but random is
(define rand (lambda () (random 12345)))
(display "a random number ")
(rand)
(display "another random number ")
(rand)

(define (rand-update x) (rand))
(display "random-update number 1 ")
(rand-update 2)
(display "random-update number 2 ")
(rand-update 3)


(define (estimate-pi-1 trials)
  (sqrt (/ 6 (monte-carlo-1 trials cesaro-test-1))))

(define (cesaro-test-1)
   (= (gcd (rand) (rand)) 1))

(define (monte-carlo-1 trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(display "estimate pi with assignment 1 ")
(estimate-pi-1 999)
(display "estimate pi with assignment 2 ")
(estimate-pi-1 999)

; (define (estimate-pi trials random-init)
;   (sqrt (/ 6 (random-gcd-test trials random-init))))
; 
; (define (random-gcd-test trials initial-x)
;   (define (iter trials-remaining trials-passed x)
;     (let ((x1 (rand-update x)))
;       (let ((x2 (rand-update x1)))
;         (cond ((= trials-remaining 0)   
;                (/ trials-passed trials))
;               ((= (gcd x1 x2) 1)
;                (iter (- trials-remaining 1)
;                      (+ trials-passed 1)
;                      x2))
;               (else
;                (iter (- trials-remaining 1)
;                      trials-passed
;                      x2))))))
;   (iter trials 0 initial-x))

(define (estimate-pi trials initial-x)
  (sqrt (/ 6 (monte-carlo trials cesaro-test initial-x))))

(define (cesaro-test x1 x2)
  (= (gcd x1 x2 1)))

(define (run-experiment experiment initial-x)
  (let ((x1 (rand-update initial-x)))
    (let ((x2 (rand-update x1)))
      (let ((x3 (rand-update x2)))
        (let ((result (experiment x1 x2)))
              (cons result x3))))))

(define (monte-carlo trials experiment initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((results (run-experiment cesaro-test x)))
      (let ((result (car results))
            (new-x (cdr results)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              (result
                (iter (- trials-remaining 1) (+ trials-passed 1) new-x))
              (else
                (iter (- trials-remaining 1) trials-passed new-x))))))
  (iter trials 0 initial-x))

(display "estimate pi functional 1 ")
(estimate-pi 999 7)
(display "estimate pi functional 2 ")
(estimate-pi 999 3)
