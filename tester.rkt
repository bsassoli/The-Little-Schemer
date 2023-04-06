#lang racket/base

(require rackunit/text-ui "./tests/ch1-tests.rkt" "./tests/ch2-tests.rkt" "./tests/ch3-tests.rkt"  "./tests/ch4-tests.rkt")

(run-tests ch1-tests 'normal)
(run-tests ch2-tests 'normal)
(run-tests ch3-tests 'normal)
(run-tests ch4-tests 'normal)

