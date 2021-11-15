#lang racket

(provide add-me sub-me)

(define add-me
  (lambda (x y)
    (display "HI!")
    (add-me x y)))

(display "Side effects!")
(add-me 3 2)

(define sub-me
  (lambda (x y)
    (- x y)))
