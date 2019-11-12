(library (chez-stats manipulation)
  (export drop-columns
	  rename-columns
	  select-columns)

  (import (chezscheme)
	  (chez-stats assertions))

  (define (header-indices row1 columns)
    ;; columns is either a list of names or list of indices
    (define hdr (map cons row1 (enumerate row1)))
    (if (for-all integer? columns)
	columns
	(map (lambda (x) (cdr (assoc x hdr))) columns)))

  (define (select-columns-actual ls header-indices)
    (map (lambda (x) (map (lambda (y) (list-ref x y)) header-indices)) ls))

  (define (rename-columns ls names)
    ;; names is of form '((old-name1 . new-name1) (old-name2 . new-name2))
    (define (replace-names row1 names)
      (map (lambda (x)
	     (let ([names-match (assoc x names)])
	       (if names-match
		   (cdr names-match)
		   x)))
	   row1))
    (let ([proc-string "(rename-columns ls names)"])
      (check-list-of-lists ls proc-string)
      (check-pairs names proc-string)
      (let ([row1 (car ls)])
	(check-names-in-header row1 (map car names) proc-string)
	(cons (replace-names row1 names) (cdr ls)))))

  (define (select-columns ls columns)
    ;; columns is either a list of names or list of indices
    (let ([proc-string "(select-columns ls columns)"])
      (check-list-of-lists ls proc-string)
      (let ([row1 (car ls)])
	(if (for-all integer? columns)
	    (check-indices row1 columns proc-string)
	    (check-names-in-header row1 columns proc-string))
	(select-columns-actual ls (header-indices row1 columns)))))

  (define (drop-columns ls columns)
    ;; columns is either a list of names or list of indices
    (let ([proc-string "(drop-columns ls columns)"])
      (check-list-of-lists ls proc-string)
      (let ([row1 (car ls)])
	(if (for-all integer? columns)
	    (check-indices row1 columns proc-string)
	    (check-names-in-header row1 columns proc-string))
	(let* ([all-idx (iota (length row1))]
	       [hdr-idx-drop (header-indices row1 columns)]
	       [hdr-idx-keep (filter (lambda (x) (not (member x hdr-idx-drop))) all-idx)])
	  (select-columns-actual ls hdr-idx-keep)))))

  )

;; create-column
;; select/drop columns
;; rename columns
;; select rows (subset)
;; order rows (ascending and descending)
;; join rows
;; group by and summarize

;; https://stackoverflow.com/questions/23380385/how-do-i-get-the-sum-of-all-elements-10-in-a-given-list-using-chez-scheme

;; ;;  (system "curl https://raw.githubusercontent.com/pandas-dev/pandas/master/doc/data/tips.csv >> tips.csv")
;; (define test (read-csv "tips.csv"))

;; (define a '((1 2 3) (4 5 6) (7 8 9)))
;; (define b (cons '(col1 col2 col3) a))
;; (define d '((4 5 6) (1 2 3) (7 8 9)))

;; ;; core logic for sorting list of lists
;; ;; split apply combine to sort on multiple dimensions
;; ;; need to figure out filtering first
;; (sort (lambda (x y)(< (car x)(car y))) d)

;; ;; core logic for filtering on one column
;; (filter (lambda (x) (> (list-ref x 2) 5)) a)

;; ;; core logic for mutating on two columns
;; (map (lambda (x y) (reverse (cons y (reverse x))))
;;      a
;;      (map (lambda (x) (+ (list-ref x 0) (list-ref x 1))) a))




 
