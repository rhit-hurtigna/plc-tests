#lang racket

(provide add-me sub-me test)

(define add-me
  (lambda (x y)
    (display "HI!")
    3))

(define sub-me
  (lambda (x y)
    1))

(define test #f)

(set! test #t)
