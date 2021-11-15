#lang racket

(provide add-me sub-me)

(define add-me
  (lambda (x y)
    3))

(define sub-me
  (lambda (x y)
    (- x y)))
