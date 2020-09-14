#lang racket/base
 
(require rackunit "./exercises.scheme")


(check-equal?
  (sum-of-squares-of-two-larger 1 2 3)
  13 
  "a < b < c")

(check-equal?
  (sum-of-squares-of-two-larger 3 4 5)
  41
  "a < b < c (2)")

(check-equal?
  (sum-of-squares-of-two-larger 5 4 3)
  41
  "a > b > c")

(check-equal?
  (sum-of-squares-of-two-larger 3 3 3)
  18
  "a == b == c")

(check-equal?
  (sum-of-squares-of-two-larger 3 2 2)
  13
  "b = c small")

(check-equal?
  (sum-of-squares-of-two-larger 2 2 3)
  13
  "a = b small")

(check-equal?
  (sum-of-squares-of-two-larger 2 3 2)
  13
  "a = c small")

(check-equal?
  (sum-of-squares-of-two-larger 3 2 3)
  18
  "a = c large")

(check-equal?
  (sum-of-squares-of-two-larger 3 3 2)
  18
  "a = b large")

(check-equal?
  (sum-of-squares-of-two-larger 2 3 3)
  18
  "b = c large")

; Section 1.2.4

(check-equal?
  (fast-expt 2 3)
  8
  "2^3 = 8")

(write ".")
