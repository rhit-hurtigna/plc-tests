#lang racket

(require "../testcode-base.rkt")
(require "assignment-2.rkt")

; To use these tests:
;   fill out the stubs in assignment-2.rkt
;   run this file in DrRacket
;   (r) or (run-all)
;   (run-test test-name) to run a portion of the tests

; If you find errors, fix them, run this file again, and type (r) again.

(define test (make-test
  (add-me
   ((add-me 1 2) 3 1))
  (sub-me)
))
