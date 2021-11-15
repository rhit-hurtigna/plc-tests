#lang racket

(require "../testcode-base.rkt")
(require "assignment-1.rkt")

(define test
  (make-test
   (add-me
    ((add-me 1 2) 3 2)
    ((add-me 5 4) 9 2)
    ((add-me -3 -4) -7 3))
   (sub-me
    ((sub-me 4 2) 2 2)
    ((sub-me 7 10) -3 2)
    ((sub-me 4 100) < 0 5))))
