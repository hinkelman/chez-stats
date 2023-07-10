#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2020 Travis Hinkelman
;; SPDX-License-Identifier: MIT
#!r6rs

(import (srfi :64 testing)
        (chez-stats delimited))

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

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
