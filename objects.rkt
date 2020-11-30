#lang sicp
(newline)

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define mynum (make-from-real-imag 2 3))

(mynum 'real-part)
(mynum 'imag-part)
(mynum 'magnitude)




; (define (Triangle base hypotenuse)
(define (Square length)
  (define (dispatch op)
    (cond ((eq? op 'perimeter) (* length 4))
          ((eq? op 'area) (square length))
          (else
           (error "Unknown op -- Square" op))))
  dispatch)

(define (Circle r)
  (define (dispatch op)
    (cond ((eq? op 'perimeter) (* r 6.3))
          ((eq? op 'area) (* (square r) 3.14159268))
          (else
           (error "Unknown op -- Square" op))))
  dispatch)

(newline)
(define asquare (Square 3))
(asquare 'perimeter)
(asquare 'area)
(define acircle (Circle 3))
(acircle 'perimeter)
(acircle 'area)

(define bigqsquare (Square 6))

(map (lambda (x) (x 'perimeter)) (list asquare acircle bigqsquare))
(map (lambda (x) (x 'area)) (list asquare acircle bigqsquare))


