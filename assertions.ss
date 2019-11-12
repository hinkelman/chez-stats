(library (chez-stats assertions)
  (export
   check-list
   check-quantile-type
   check-p
   check-p-exclusive
   check-positive-integer
   check-real
   check-positive-real
   check-real-gte-zero
   check-list-of-lists
   check-pairs
   check-indices
   check-names-in-header)

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

  (define (check-positive-real x x-name who)
    (unless (and (real? x) (> x 0))
      (assertion-violation who (string-append x-name " is not a positive real number"))))

  (define (check-real-gte-zero x x-name who)
    (unless (and (real? x) (>= x 0))
      (assertion-violation who (string-append x-name " is not a real number >= 0"))))

  (define (check-list-of-lists ls who)
    (unless (and (list? ls) (list? (car ls)))
      (assertion-violation who "ls is not a list of lists")))

  (define (check-pairs names who)
    (unless (for-all pair? names)
      (assertion-violation who "names not of form '((old-name1 . new-name1) (old-name2 . new-name2))")))

  (define (check-indices row1 indices who)
    (define all-idx (enumerate row1))
    (unless (for-all (lambda (x) (member x all-idx)) indices)
      (assertion-violation who "column indices are out of range")))

  (define (check-names-in-header row1 names who)
    (unless (for-all (lambda (x) (member x row1)) names)
      (assertion-violation who "names not found in header")))
  )

;; considered adding checks for duplicates in (chez-stats manipulation)
;; b/c duplicate header names will lead to bad behavior
;; decided it was too heavy-handed but leaving these here for now
;;
;; ;; https://stackoverflow.com/questions/8382296/scheme-remove-duplicated-numbers-from-list
;; (define (remove-duplicates ls)
;;   (cond [(null? ls)
;; 	   '()]
;; 	  [(member (car ls) (cdr ls))
;; 	   (remove-duplicates (cdr ls))]
;; 	  [else
;; 	   (cons (car ls) (remove-duplicates (cdr ls)))]))
;;
;; (define (check-contains-duplicates ls ls-name who)
;;   (unless (= (length ls) (length (remove-duplicates ls)))
;;     (assertion-violation who (string-append ls-name " contains duplicate elements"))))
;;
;; (define (check-duplicated ls1 ls2 ls1-name ls2-name who)
;;   (unless (for-all (lambda (x) (not (member x ls))) ls2)
;;     (assertion-violation who (string-append ls2-name " duplicates elements in " ls1-name))))



