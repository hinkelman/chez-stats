#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2020 Travis Hinkelman
;; SPDX-License-Identifier: MIT
#!r6rs

(import (srfi :64 testing)
        (chez-stats))

;; delimited; only testing comma here (for now?)
(test-begin "delimited-test")
(define example-list (list
		      (list "col1" "col2" "col3" "col4")
		      (list 10.02 #\A "1,000" "Glen \"Big Baby\" Davis")
		      (list 1/3 #\B "1000" "Earvin \"Magic\" Johnson")))
(write-delim example-list "example.csv")
(test-error (write-delim example-list "example.csv" #\, #f))
;; if example-list was all string wouldn't need to jump through these extra hoops
(define example-list2 (read-delim "example.csv"))
(write-delim example-list2 "example2.csv")
(test-equal example-list2 (read-delim "example2.csv"))
(delete-file "example.csv")
(delete-file "example2.csv")
(test-end "delimited-test")

;; random-variates

(test-begin "bernoulli-test")
(define bernoulli-list (random-sample 1e5 'bernoulli 0.2))
(test-approximate 0.2 (mean bernoulli-list) 0.01)
(test-approximate 0.16 (variance bernoulli-list) 0.008)
(test-error (random-bernoulli 2 1.2))
(test-end "bernoulli-test")

(test-begin "beta-test")
(define beta-list (random-sample 1e5 'beta 1 3))
(test-approximate 0.25 (mean beta-list) 0.0125)
(test-approximate 0.037 (variance beta-list) 0.00185)
(test-error (random-beta -7 3))
(test-error (random-beta 1 'a))
(test-end "beta-test")

(test-begin "beta-binomial-test")
(define beta-binomial-list (random-sample 1e5 'beta-binomial 10 0.5 1.0001))
(test-approximate 5 (mean beta-binomial-list) 0.25)
(test-approximate 2.5 (variance beta-binomial-list) 0.125)
(test-error (random-beta-binomial 10.3 0.5 5))
(test-error (random-beta-binomial 10 0 5))
(test-error (random-beta-binomial 10 0.8 0))
(test-error (random-beta-binomial 10 0.8 11))
(test-end "beta-binomial-test")

(test-begin "binomial-test")
(define binomial-list (random-sample 1e5 'binomial 10 0.5))
(test-approximate 5 (mean binomial-list) 0.25)
(test-approximate 2.5 (variance binomial-list) 0.125)
(test-error (random-binomial -2 0.5))
(test-error (random-bionmial 2 1.2))
(test-end "binomial-test")

(test-begin "exponential-test")
(define exponential-list (random-sample 1e5 'exponential 10))
(test-approximate 10 (mean exponential-list) 0.5)
(test-approximate 100 (variance exponential-list) 5)
(test-error (random-exponential -2))
(test-end "exponential-test")

(test-begin "gamma-test")
(define gamma-list (random-sample 1e5 'gamma 10 2))
(test-approximate 5 (mean gamma-list) 0.25)
(test-approximate 2.5 (variance gamma-list) 0.125)
(test-error (random-gamma -7 2))
(test-error (random-gamma 10 "travis"))
(test-end "gamma-test")

(test-begin "geometric-test")
(define geometric-list (random-sample 1e5 'geometric 0.2))
(test-approximate 5 (mean geometric-list) 0.25)
(test-approximate 20 (variance geometric-list) 1)
(test-error (random-geometric 1))
(test-end "geometric-test")

(test-begin "lognormal-test")
(define lognormal-list (random-sample 1e5 'lognormal 2 1))
(test-approximate 12.28 (mean lognormal-list) 0.614)
(test-approximate 16 (standard-deviation lognormal-list) 0.8)
(test-error (random-lognormal "a" 1))
(test-error (random-lognormal 2 -1))
(test-end "lognormal-test")

(test-begin "multinomial-test")
(define multinomial-list (random-multinomial 1e5 '(0.05 0.45 0.45 0.05)))
(define ml-sum (apply + multinomial-list))
(test-equal 100000 ml-sum)
(test-approximate 1 (apply + (map (lambda (x) (/ x ml-sum)) multinomial-list)) 0.01)
(test-equal 100 (apply + (random-multinomial 100 '(50 50))))
(test-equal '(100) (random-multinomial 100 '(1)))
(test-error (random-multinomial 'a '(0.5 0.5)))
(test-error (random-multinomial 100 'b))
(test-end "multinomial-test")

(test-begin "negative-binomial-test")
(define negative-binomial-list (random-sample 1e5 'negative-binomial 10 0.2))
(test-approximate 40 (mean negative-binomial-list) 2)
(test-approximate 201 (variance negative-binomial-list) 10.05)
(test-error (random-negative-binomial 10 0))
(test-error (random-negative-binomial 0 0.2))
(test-end "negative-binomial-test")

(test-begin "normal-test")
(define normal-list (random-sample 1e5 'normal 42 5))
(test-approximate 42 (mean normal-list) 2.1)
(test-approximate 5 (standard-deviation normal-list) 0.25)
(test-error (random-normal "a" 5))
(test-error (random-normal 2 -5))
(test-end "normal-test")

(test-begin "pareto-test")
(define pareto-list (random-sample 1e5 'pareto 5))
(test-approximate 1.25 (mean pareto-list) 0.0625)
(test-approximate 0.104 (variance pareto-list) 0.005)
(test-error (random-pareto -2))
(test-end "pareto-test")

(test-begin "poisson-test")
(define poisson-list (random-sample 1e5 'poisson 7))
(test-approximate 7 (mean poisson-list) 0.35)
(test-approximate 7 (variance poisson-list) 0.35)
(test-error (random-poisson -10))
(test-end "poisson-test")

(test-begin "uniform-test")
(define uniform-list (random-sample 1e5 'uniform -10 7))
(test-approximate -10 (apply min uniform-list) 0.01)
(test-approximate 7 (apply max uniform-list) 0.01)
(test-error (random-uniform 'a 1))
(test-error (random-uniform 2 1))
(test-end "uniform-test")

;; statistics

(test-begin "count-unique-test")
(test-equal '((1 . 2) (2 . 2) (3 . 1) (4 . 1))
  (count-unique '(1 2 3 4 2 1)))
(test-equal '((1 . 1) (1.1 . 3) (2 . 1) (2.2 . 1))
  (count-unique '(1.1 1 2.2 2 1.1 1.1)))
(test-equal '((1/2 . 3) (1 . 2) (2 . 1))
 (count-unique '(0.5 1/2 #e0.5 1 1 2)))
(test-error (count-unique '("a" "b" "b" "a")))
(test-end "count-unique-test")

(test-begin "cumulative-sum-test")
(test-equal '(1 3 6 10 15) (cumulative-sum '(1 2 3 4 5)))
(test-equal '(5 9 12 14 15) (cumulative-sum '(5 4 3 2 1)))
(test-error (cumulative-sum '()))
(test-end "cumulative-sum-test")

(test-begin "correlation-test")
(define x '(86 97 99 100 101 103 106 110 112 113 86))
(define y '(2 20 28 27 50 29 7 17 6 12 1))
(test-approximate 0.156 (correlation x y 'pearson) 0.001)
(test-approximate 0.114 (correlation x y 'spearman) 0.001)
(test-approximate 0.073 (correlation x y 'kendall) 0.001)
(test-approximate 1.0 (correlation (iota 10) (iota 10) 'pearson) 0.00001)
(test-approximate 1.0 (correlation (iota 10) (iota 10) 'spearman) 0.00001)
(test-approximate 1.0 (correlation (iota 10) (iota 10) 'kendall) 0.00001)
(test-approximate -1.0 (correlation (iota 10) (map - (iota 10)) 'pearson) 0.00001)
(test-approximate -1.0 (correlation (iota 10) (map - (iota 10)) 'spearman) 0.00001)
(test-approximate -1.0 (correlation (iota 10) (map - (iota 10)) 'kendall) 0.00001)
(test-error (correlation '(1 2 3) '(1 2) 'pearson))
(test-error (correlation '(1 2 3) 7 'pearson))
(test-error (correlation '(a) '(1 2) 'pearson))
(test-error (correlation '(1 2 3) '(1 2 3) 'test))
(test-error (correlation '(1 2 3) '(1 2 3) "kendall"))
(test-end "correlation-test")

(test-begin "diff-test")
(test-equal '(2 4 6) (diff '(1 3 7 13)))
(test-equal '(0 0) (diff '(1 1 1)))
(test-equal '(30 -30) (diff '(-10 20 -10)))
(test-error (diff "test"))
(test-end "diff-test")

(test-begin "kurtosis-test")
(test-assert (= 17/10 (kurtosis '(1 2 3 4 5))))
(test-assert (= 51/25 (kurtosis '(1 2 2 3 3 3))))
(test-error (kurtosis '()))
(test-error (kurtosis "a"))
(test-end "kurtosis-test")

(test-begin "mean-test")
(test-assert (= 3 (mean '(1 2 3 4 5))))
(test-assert (= 0 (mean '(-10 0 10))))
(test-assert (= 27.5 (exact->inexact (mean '(1 2 3 4 5 150)))))
(test-error (mean '()))
(test-error (mean "a"))
(test-end "mean-test")

(test-begin "median-test")
(test-assert (= 3.5 (median '(1 2 3 4 5 6))))
(test-error (median '()))
(test-error (median '("a")))
(test-end "median-test")

(test-begin "mode-test")
(test-equal '(1 2) (mode '(1 1 1 2 2 2)))
(test-equal '(4) (mode '(1 2 3 3 4 4 4)))
(test-error (mode 'a))
(test-end "mode-test")

(test-begin "quantile-test")
(test-assert (= 3 (quantile '(1 2 3 4 5 6) 0.5 1)))
(test-assert (= 3.0 (quantile '(1 2 3 4 5 6) 0.5 4)))
(test-assert (= 3.5 (quantile '(1 2 3 4 5 6) 0.5 8)))
(test-assert (= 1.125 (quantile '(1 2 3 4 5 6) 0.025 7)))
(test-error (quantile '(1 2) 0.5 100))
(test-error (quantile '(1 2) 2 7))
(test-end "quantile-test")

(test-begin "rank-test")
(test-equal '(3 2 4 1 5) (rank '(30 20 50 10 60)))
(test-equal '(4 2 5 1 8 5 2 5) (rank '(30 20 50 10 60 50 20 50)))
(test-equal '(4 5/2 6 1 8 6 5/2 6) (rank '(30 20 50 10 60 50 20 50) 'mean))
(test-equal '(4 3 7 1 8 7 3 7) (rank '(30 20 50 10 60 50 20 50) 'max))
(test-end "rank-test")

(test-begin "rep-test")
(test-equal '(1 2 1 2 1 2) (rep 3 '(1 2) 'times))
(test-equal '(1 1 1 2 2 2) (rep 3 '(1 2) 'each))
(test-equal '("test" "this" "test" "this") (rep 2 '("test" "this") 'times))
(test-error (rep 1.5 '(10) 'each))
(test-error (rep 2 10 'each))
(test-error (rep 2 '(10) "each"))
(test-end "rep-test")

(test-begin "rle-test")
(test-equal '((1 . 1) (2 . 1) (3 . 1) (4 . 1) (2 . 1) (1 . 1))
  (rle '(1 2 3 4 2 1)))
(test-equal '((1 . 3) (2 . 1) (1 . 2) (2 . 1))
  (rle '(1 1 1 2 1 1 2)))
(test-equal '((3 . 2) (1 . 2) (2 . 3))
  (rle '(3 3 1 1 2 2 2)))
(test-error (rle '("a" "b" "b" "a")))
(test-end "rle-test")

(test-begin "sign-test")
(test-equal '(-1 0 1) (map sign '(-10 0 10)))
(test-end "sign-test")

(test-begin "skewness-test")
(test-assert (= 0 (skewness '(1 2 3 4 5))))
(test-assert (= -0.6 (skewness '(1 2 2 3 3 3 4 4 4 4))))
(test-error (skewness '()))
(test-error (skewness '("a" 1)))
(test-end "skewness-test")

(test-begin "standard-deviation-test")
(test-assert (= 1.8708286933869707 (standard-deviation '(0 1 2 3 4 5))))
(test-assert (= 0 (standard-deviation '(1 1 1))))
(test-error (standard-deviation '(1 'a)))
(test-end "standard-deviation-test")

(test-begin "unique-test")
(test-equal '(1/2 1 5.2) (unique '(0.5 #e0.5 1/2 1 1 1 5.2)))
(test-equal '(0 1 2) (unique '(0 0 0 1 1 1 2)))
(test-end "unique-test")

(test-begin "variance-test")
(test-assert (= 233840.25 (variance '(1 10 100 1000))))
(test-assert (= 3.5 (variance '(0 1 2 3 4 5))))
(test-error (variance '()))
(test-end "variance-test")

(test-begin "weighted-mean-test")
(test-assert (= 7/3 (weighted-mean '(1 2 3 4 5) '(5 4 3 2 1))))
(test-assert (= 3 (weighted-mean '(1 2 3 4 5) '(2 2 2 2 2))))
(test-assert (= 13/4 (weighted-mean '(1 2 3 4 5) '(2 0 2 2 2))))
(test-error (weighted-mean '() '(1 2 3)))
(test-error (weighted-mean '(1) '(1 2 3)))
(test-error (weighted-mean '(1) '(1 2 "a")))
(test-end "weighted-mean-test")

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
