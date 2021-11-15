#lang racket

(provide add-me sub-me individual-test get-weights)

(define add-me
  (lambda (x y)
    (display "HI!")
    3))

(define sub-me
  (lambda (x y)
    1))

(define test #f)

(set! test #t)

(define individual-test
  (lambda (blah blaah blaaah)
    #t))

(set! individual-test
  (lambda (blah blaah blaaah)
    #t))

(define get-weights
  (lambda (test)
    (list)))