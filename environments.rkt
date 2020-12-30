#lang sicp
(newline)
(display "--------------------------------------------")
(newline)


;; exercise 3.10

(define (make-withdraw balance)
  ; e1, balance=?
  (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(W1 20)
(W1 10)

(define (make-withdraw-2 initial-amount)
  ; e1, initial-amount=?
  ((lambda (balance) ;e2, balance=?
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
   initial-amount))

(define W1-2 (make-withdraw-2 100))

(W1-2 20)
(W1-2 10)
