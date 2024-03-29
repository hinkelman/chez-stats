(library (chez-stats assertions)
  (export
   check-empty-list
   check-is-list
   check-list
   check-quantile-type
   check-p
   check-p-exclusive
   check-positive-integer
   check-real
   check-positive-real
   check-real-gte-zero
   check-integer-gte-zero)

  (import (chezscheme))

  ;; generally prodecures in statistics.sls work on non-empty lists of real numbers
  (define (check-list lst lst-name who)
    (check-is-list lst lst-name who)
    (check-empty-list lst lst-name who)
    (check-all-real lst lst-name who))

  (define (check-is-list lst lst-name who)
    ;; not a great name but decomposing check-list and not wanting to break a bunch of code
    (unless (list? lst)
      (assertion-violation who (string-append lst-name " is not a list"))))

  (define (check-empty-list lst lst-name who)
    (when (null? lst)
      (assertion-violation who (string-append lst-name " is empty"))))
  
  (define (check-quantile-type type who)
    (unless (and (and (> type 0) (< type 10)) (integer? type))
      (assertion-violation who "type is not an integer from 1-9")))

  (define (check-p p who)
    (unless (and (and (>= p 0) (<= p 1)) (real? p))
      (assertion-violation who "p is not a real number in [0,1]")))

  (define (check-p-exclusive p who)
    (unless (and (and (> p 0) (< p 1)) (real? p))
      (assertion-violation who "p is not a real number in (0,1)")))

  (define (check-positive-integer x x-name who)
    (unless (and (> x 0) (integer? x))
      (assertion-violation who (string-append x-name " is not a positive integer"))))

  (define (check-real x x-name who)
    (unless (real? x)
      (assertion-violation who (string-append x-name " is not a real number"))))

  (define (check-all-real lst lst-name who)
    (unless (for-all real? lst)
      (assertion-violation who (string-append "at least one element of " lst-name " is not a real number"))))

  (define (check-positive-real x x-name who)
    (unless (and (real? x) (> x 0))
      (assertion-violation who (string-append x-name " is not a positive real number"))))

  (define (check-real-gte-zero x x-name who)
    (unless (and (real? x) (>= x 0))
      (assertion-violation who (string-append x-name " is not a real number >= 0"))))

  (define (check-integer-gte-zero x x-name who)
    (unless (and (integer? x) (>= x 0))
      (assertion-violation who (string-append x-name " is not an integer >= 0"))))
  )




