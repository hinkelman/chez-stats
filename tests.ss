;; run this file from terminal with
;; > chez path/to/tests.ss

(import (chez-stats statistics)
	(chez-stats random-variates)
	(srfi s64 testing))

(test-begin "bernoulli-test")
(define bernoulli-list (random-bernoulli 1e5 0.2))
(test-approximate 0.2 (mean bernoulli-list) 0.01)
(test-approximate 0.16 (variance bernoulli-list) 0.008)
(test-error (random-bernoulli -2 0.2))
(test-error (random-bernoulli 2 1.2))
(test-end "bernoulli-test")

(test-begin "binomial-test")
(define binomial-list (random-binomial 1e5 10 0.5))
(test-approximate 5 (mean binomial-list) 0.25)
(test-approximate 2.5 (variance binomial-list) 0.125)
(test-error (random-binomial -2 10 0.5))
(test-error (random-binomial 2 -2 0.5))
(test-error (random-bionmial 2 2 1.2))
(test-end "binomial-test")

(test-begin "exponential-test")
(define exponential-list (random-exponential 1e5 10))
(test-approximate 10 (mean exponential-list) 0.5)
(test-approximate 100 (variance exponential-list) 5)
(test-error (random-exponential -2 10))
(test-error (random-exponential 2 -2))
(test-end "exponential-test")

(test-begin "normal-test")
(define normal-list (random-normal 1e5 42 5))
(test-approximate 42 (mean normal-list) 2.1)
(test-approximate 5 (standard-deviation normal-list) 0.25)
(test-error (random-normal -2 42 5))
(test-error (random-normal 2 "a" 5))
(test-error (random-normal 2 2 -5))
(test-end "normal-test")

(test-begin "poisson-test")
(define poisson-list (random-poisson 1e5 7))
(test-approximate 7 (mean poisson-list) 0.35)
(test-approximate 7 (variance poisson-list) 0.35)
(test-error (random-normal -2 7))
(test-error (random-normal 2 -10))
(test-end "poisson-test")



