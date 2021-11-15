# plc-tests

For every assignment that you've created in Gradescope, you should make a folder here called <assignment-name>, where assignment-name is what you passed to the .zip generator in the other repository. That folder needs to contain a "testcode.rkt", which should (require "../testcode-base.rkt") and (require "whatever-the-student-submission-is.rkt"). You can pass the .zip generator the -f option to tell it what the student submission file name should be; the default is assignment-name.rkt.

The testcode.rkt file should (define test (make-test suite1 suite2 ...)), where a suite is (suitename testcase1 testcase2 ...), and a testcase is either (code-to-execute expected weight) or (code-to-execute equivalent? expected weight). If equivalent? is present, success is defined as (equivalent? (lambda () code-to-execute) expected). If equivalent? is not passed, equal? is used by default. Tests can have as little as 0 suites and suites can have as little as 0 tests (though that would be entirely useless).

One thing to note is to make sure the sums of all the weights of the tests is the same as the Gradescope assignment's points. Otherwise, passing all test cases will get a grade that's not 100%.

Students can use this same structure locally. They can put their whatever-the-student-submission-is.rkt file in the same directory as testcode.rkt (it might be nice to push .rkt files containing stubs and descriptions of the requirements onto this repo), run testcode.rkt, and run (r) or (run-all) to run all tests. They can run an individual suite of tests (probably for a single function or feature) by running (run-test suite-name).
