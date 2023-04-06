#lang racket/base

(require rackunit/text-ui "./tests/ch1-tests.rkt" "./tests/ch2-tests.rkt")
(run-tests ch1-tests 'normal)
(run-tests ch2-tests 'normal)
