#lang racket

(provide add-me sub-me)

(define add-me
  (lambda (x y)
    (+ x y)))

(display "Side effect!")
(+ 2 3)

(define 1 sub-me)
