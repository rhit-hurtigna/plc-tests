#lang racket

(provide add-me sub-me)

(define add-me
  (lambda (x y)
    (+ x y)))

(define sub-me
  (lambda (x y)
    (sub-me x y)))
