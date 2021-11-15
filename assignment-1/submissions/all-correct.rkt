#lang racket

(provide add-me sub-me)

(define add-me
  (lambda (x y)
    (display "HI!")
    (+ x y)))

(display "Side effects!")
(+ 3 2)

(define sub-me
  (lambda (x y)
    (- x y)))
