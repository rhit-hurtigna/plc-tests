#lang racket

(provide make-test make-run-all run-test make-individual-test make-get-weights)

;;--------  Procedures used by the testing mechanism   ------------------

(define-syntax make-test
  (syntax-rules ()
    ([_ (name testcase ...) ...]
     [cons
      (list (syntax->datum #'name) ...)
      (list (list (make-testcase testcase) ...) ...)])))

(define-syntax make-testcase
  (syntax-rules ()
    ([_ (body expected weight)]
     [make-testcase (body equal? expected weight)])
    ([_ (body equivalent? expected weight)]
     [list (lambda () body) equivalent? expected weight (syntax->datum #'body)])))


    (define-syntax run-test
      (syntax-rules ()
        ([_ test-name]
        [let* ([test-symbol (syntax->datum #'test-name)][suite-index (index-of (car test) test-symbol)])
          (if suite-index
              (begin (run-suite test-symbol (list-ref (cdr test) suite-index)) (void))
              (printf "Test not found: ~a\n" test-symbol))])))

(define run-suite
  (lambda (name suite)
      (printf "~a: " name)
      (let loop ([suite-pointer suite] [passed #t] [score 0] [max-score 0])
        (if [null? suite-pointer]
            (begin (if passed
              (printf "All correct ~a/~a\n" score max-score)
              (printf "~a score: ~a/~a\n" name score max-score))
                   (cons score max-score))
            (let ([testcase (car suite-pointer)] [other (cdr suite-pointer)])
              (let ([student-answer ((car testcase))] [equivalent? (cadr testcase)] [expected (caddr testcase)] [test-weight (cadddr testcase)] [code (car (cddddr testcase))])
                (if [equivalent? student-answer expected]
                    (loop other passed (+ score test-weight) (+ max-score test-weight))
                    (begin
                      (when passed
                        (printf "\n~a" suite-separator))
                      (printf "Test case: ~a\nYours: ~a\nExpected: ~a\n~a" code student-answer expected suite-separator)
                      (loop other #f score (+ max-score test-weight))))))))))

(define make-run-all
  (lambda (my-test)
    (lambda ()
      (let ([test-length (length (car my-test))])
        (let loop ([index 0] [score 0] [max-score 0])
          (if [< index test-length]
            (let ([suite-name (list-ref (car my-test) index)] [test-suite (list-ref (cdr my-test) index)])
              (let ([suite-scores (run-suite suite-name test-suite)])
                (let ([suite-score (car suite-scores)] [suite-max-score (cdr suite-scores)])
                  (loop (add1 index) (+ suite-score score) (+ max-score suite-max-score)))))
            (printf "~aTotal score: ~a/~a" suite-separator score max-score)))))))

(define suite-separator "----------\n")

(define make-individual-test
  (lambda (my-test)
    (lambda (suite-index test-index)
      (let ([suite-name (list-ref (car my-test) suite-index)] [suite (list-ref (cdr my-test) suite-index)])
        (let ([testcase (list-ref suite test-index)])
          (let ([student-answer ((car testcase))] [equivalent? (cadr testcase)] [expected (caddr testcase)] [test-weight (cadddr testcase)] [code (car (cddddr testcase))])
            (let ([score (if [equivalent? student-answer expected] test-weight 0)])
              (list score code student-answer expected))))))))

(define make-get-weights
  (lambda (my-test)
    (lambda ()
      (let loop ([suites-ptr (cdr my-test)])
        (if [null? suites-ptr]
            (list)
            (let ([suite (car suites-ptr)] [other (cdr suites-ptr)])
              (cons
              (let inner-loop ([tests-ptr suite])
                (if [null? tests-ptr]
                    (list)
                    (let ([my-test (car tests-ptr)] [other-tests (cdr tests-ptr)])
                      (cons (caddr my-test) (inner-loop other-tests)))))
              (loop other))))))))
