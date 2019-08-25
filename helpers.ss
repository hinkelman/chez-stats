(library (statistics helpers)
  (export
   list-check
   quantile-type-check
   check-p
   build-random-list)

  (import (chezscheme))

  (define (list-check ls who)
    (unless (for-all real? ls)
      (assertion-violation who "all elements of list must be real numbers;" ls))
    (when (null? ls)
      (assertion-violation who "list is empty;" ls)))

  (define (quantile-type-check type who)
    (when (or (< type 1) (> type 9) (not (integer? type)))
      (assertion-violation who "type must be integer from 1-9;" type)))

  (define (check-p p who)
    (when (or (< p 0) (> p 1))
      (assertion-violation who "p is outside [0,1];" p)))

  (define (build-random-list n proc)
    (define (iterate result i)
      (if (= i n)
	  result
	  (iterate (cons (proc) result) (add1 i))))
    (iterate '() 0))
  )

