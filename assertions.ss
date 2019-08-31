;; doens't make sense to print full object b/c it might be really long list
;; would prefer to drop the with irritant part
;; just discovered that irritation part is optional

;; how do other people do it
;; pulled this out of statistics package for now
;; not critical until I understand how to do it well

(library (chez-stats assertions)
  (export
   check-list
   check-quantile-type
   check-p
   check-positive-integer
   check-real)

  (import (chezscheme))

  (define (check-list ls ls-name who)
    (unless (list? ls)
      (assertion-violation who (string-append ls-name " is not a list")))
    (unless (for-all real? ls)
      (assertion-violation who (string-append "at least one element of " ls-name " is not a real number")))
    (when (null? ls)
      (assertion-violation who (string-append ls-name " is empty"))))

  (define (check-quantile-type type who)
    (unless (and (and (> type 0) (< type 10)) (integer? type))
      (assertion-violation who "type is not an integer from 1-9")))

  (define (check-p p who)
    (unless (and (and (>= p 0) (<= p 1)) (real? p))
      (assertion-violation who "p is not a real number from 0-1")))

  (define (check-positive-integer x x-name who)
    (unless (and (> x 0) (integer? x))
      (assertion-violation who (string-append x-name " is not a positive integer"))))

  (define (check-real x x-name who)
    (unless (real? x)
      (assertion-violation who (string-append x-name " is not a real number"))))		   
  )

