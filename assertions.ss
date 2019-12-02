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
   ;check-list-of-lists
   check-names-unique
   check-names-symbol
   check-names
   check-names-duplicate
   check-new-names
   check-name-pairs
   check-alist
   check-values
   check-procedure
   remove-duplicates)

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

  ;; (define (check-list-of-lists ls who)
  ;;   (unless (and (list? ls) (list? (car ls)))
  ;;     (assertion-violation who "ls is not a list of lists")))

  (define (check-names-unique names who)
    (unless (= (length names) (length (remove-duplicates names)))
      (assertion-violation who "names are not unique")))

  (define (check-names-symbol names who)
    (unless (for-all (lambda (name) (symbol? name)) names)
      (assertion-violation who "names are not symbols")))

  (define (check-names names who)
    (check-names-symbol names who)
    (check-names-unique names who))

  (define (check-names-duplicate old-names new-names who)
    (unless (for-all (lambda (new-name) (not (member new-name old-names))) new-names)
      (assertion-violation who "new names duplicate existing names")))

  (define (check-new-names old-names new-names who)
    (check-names new-names who)
    (check-names-duplicate old-names new-names who))
  
  (define (check-name-pairs current-names name-pairs who)
    ;; not very thorough checking of ways a name-pair could be malformed
    (unless (for-all pair? name-pairs)
      (assertion-violation who "names not of form '((old-name1 new-name1) (old-name2 new-name2))"))
    (let ([new-names (map cadr name-pairs)])
      (check-new-names current-names new-names who)))

  ;; https://stackoverflow.com/questions/8382296/scheme-remove-duplicated-numbers-from-list
  (define (remove-duplicates ls)
    (cond [(null? ls)
	   '()]
	  [(member (car ls) (cdr ls))
	   (remove-duplicates (cdr ls))]
	  [else
	   (cons (car ls) (remove-duplicates (cdr ls)))]))

  ;; https://stackoverflow.com/questions/28753729/how-to-manually-flatten-a-list-in-racket-scheme
  (define (flatten obj)
    (cond [(null? obj) '()]
	  [(list? obj)
	   (append (flatten (car obj))
		   (flatten (cdr obj)))]
	  [else (list obj)]))

  (define (simple-list? ls)
    (equal? ls (flatten ls)))

  (define (same-length? len ls)
    (= len (length ls)))
  
  ;; does this add too much overhead?
  ;; lots of checking that will be performed every time a dataframe is created
  (define (check-alist alist who)
    (unless (list? alist)
      (assertion-violation who "alist is not a list"))
    (when (null? alist)
      (assertion-violation who "alist is empty"))
    (unless (for-all (lambda (col) (pair? col)) alist)
      (assertion-violation who "alist is not a list of pairs"))
    (let ([names (map car alist)])
      (check-names-symbol names who)
      (check-names-unique names who))
    (unless (for-all (lambda (col) (list? (cadr col))) alist)
      (assertion-violation who "values are not a list"))
    (unless (for-all (lambda (col) (simple-list? (cadr col))) alist)
      (assertion-violation who "values are not a simple list"))
    (let ([num-rows (length (cadar alist))])
      (unless (for-all (lambda (col) (same-length? num-rows (cadr col))) alist)
	(assertion-violation who "columns not all same length"))))

  (define (check-values num-rows new-values who)
    (unless (list? new-values)
      (assertion-violation who "values are not a list"))
    (unless (simple-list? new-values)
      (assertion-violation who "values are not a simple list"))
    (unless (same-length? num-rows new-values)
      (assertion-violation who (string-append "values length should be  "
					      (number->string num-rows)
					      ", not "
					      (number->string (length new-values))))))

  (define (check-procedure proc who)
    (unless (procedure? proc)
      (assertion-violation who "proc is not a procedure")))

  ;; (define (check-indices row1 indices who)
  ;;   (define all-idx (enumerate row1))
  ;;   (unless (for-all (lambda (index) (member index all-idx)) indices)
  ;;     (assertion-violation who "column indices are out of range")))

  ;; (define (check-names-in-header row1 names who)
  ;;   (unless (for-all (lambda (name) (member name row1)) names)
  ;;     (assertion-violation who "names not found in header")))


  )




