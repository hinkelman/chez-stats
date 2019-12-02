(library (chez-stats manipulation)
  (export ->
	  drop-columns
	  rename-columns
	  select-columns
	  modify-columns
	  create-column)

  (import (chezscheme)
	  (chez-stats assertions))

  (define (column-indices row1 columns)
    ;; columns is either a list of names or list of indices
    (define hdr (map cons row1 (enumerate row1)))
    (if (for-all integer? columns)
	columns
	(map (lambda (column-name) (cdr (assoc column-name hdr))) columns)))

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

  ;; ;; returns list
  ;; (define (select-columns-by-index ls idx)
  ;;   (map (lambda (row) (list-ref row idx)) ls))

  (define (select-columns-by-index-list ls idx-ls)
    (map (lambda (row)
	   (map (lambda (idx)
		  (list-ref row idx))
		idx-ls))
	 ls))
  
  (define (select-columns ls columns)
    ;; columns is either a list of names or list of indices
    (let ([proc-string "(select-columns ls columns)"])
      (check-list-of-lists ls proc-string)
      (let ([row1 (car ls)])
	(if (for-all integer? columns)
	    (check-indices row1 columns proc-string)
	    (check-names-in-header row1 columns proc-string))
	(select-columns-by-index-list ls (column-indices row1 columns)))))

  (define (drop-columns ls columns)
    ;; columns is either a list of names or list of indices
    (let ([proc-string "(drop-columns ls columns)"])
      (check-list-of-lists ls proc-string)
      (let ([row1 (car ls)])
	(if (for-all integer? columns)
	    (check-indices row1 columns proc-string)
	    (check-names-in-header row1 columns proc-string))
	(let* ([all-idx (enumerate row1)]
	       [hdr-idx-drop (column-indices row1 columns)]
	       [hdr-idx-keep (filter (lambda (x) (not (member x hdr-idx-drop))) all-idx)])
	  (select-columns-by-index-list ls hdr-idx-keep)))))

  (define (cons-end ls x)
    (reverse (cons x (reverse ls))))

  (define (modify-columns-by-index-list ls idx-ls proc)
    (let ([all-idx (enumerate (car ls))])
      (map (lambda (row)
	     (map (lambda (idx)
		    (if (member idx idx-ls)
			(proc (list-ref row idx))
			(list-ref row idx)))
		  all-idx))
	   ls)))

  ;; proc can only accept one argument
  (define (modify-columns ls columns proc)
    ;; columns is either a list of names or list of indices
    (let ([proc-string "(modify-columns ls columns)"])
      (check-list-of-lists ls proc-string)
      (check-all-same-length ls proc-string)
      (let ([row1 (car ls)])
	(cond [(for-all integer? columns)
	       (check-indices row1 columns proc-string)
	       (modify-columns-by-index-list ls (column-indices row1 columns) proc)]
	      [else
	       (check-names-in-header row1 columns proc-string)
	       (cons row1 (modify-columns-by-index-list (cdr ls) (column-indices row1 columns) proc))]))))

  (define (create-column-by-index-list ls idx-ls proc)
    (map (lambda (row)
	   (cons-end row (proc
			  (map (lambda (idx) (list-ref row idx))
				    idx-ls))))
	 ls))
  
  ;; proc can only take one argument and that argument needs to be a list
  (define (create-column ls columns name proc)
    ;; columns is either a list of names or list of indices
    (let ([proc-string "(create-column ls columns proc)"])
      (check-list-of-lists ls proc-string)
      (check-all-same-length ls proc-string)
      (let ([row1 (car ls)])
	(cond [(for-all integer? columns)
	       (check-indices row1 columns proc-string)
	       (create-column-by-index-list ls (column-indices row1 columns) proc)]
	      [else
	       (check-names-in-header row1 columns proc-string)
	       (cons (cons-end row1 name) (create-column-by-index-list (cdr ls) (column-indices row1 columns) proc))]))))
  )

;; (define a '((1 2 3) (4 5 6) (7 8 9)))
;; (define b (cons '(col1 col2 col3) a))
;; (create-column a '(1 2) '() (lambda (x) (apply + x)))
;; (create-column a '(2) '() (lambda (x) (log (car x))))
;; (create-column b '(col1 col2 col3) 'sum (lambda (x) (apply + x)))
;; (create-column b '(col1 col2 col3) 'mean (lambda (x) (/ (apply + x) (length x))))
;; ;; next two lines are same;; up to programmer to handle list-referencing in proc
;; (create-column b '(col1 col2 col3) 'col4 (lambda (x) (* (car x) (/ (cadr x) (caddr x)))))
;; (create-column b '(col1 col2 col3) 'col4 (lambda (x) (* (list-ref x 0) (/ (list-ref x 1) (list-ref x 2)))))


;; data-frame implementation idea
;; main question right now is whether to use mutable hashtable or not
;; can't use thread-first operator with mutatable because nothing passed to next operation
(define-record-type data-frame (fields ht)
		    (protocol
		     (lambda (new)
		       (lambda ()
			 (new (hashtable-copy (make-hashtable symbol-hash symbol=?)))))))
;; (define test (make-data-frame))
;; (hashtable-set! (data-frame-ht test) 'a '(1 2 3))
;; (hashtable-set! (data-frame-ht test) 'b '(4 5 6))
;; (hashtable-entries (data-frame-ht test))
;; (hashtable-contains? (data-frame-ht test) 'a)
;; (hashtable-mutable? (data-frame-ht test))
;; (list? (hashtable-ref (data-frame-ht test) 'a '()))
;; (hashtable-keys (data-frame-ht test))

(define-record-type data-frame (fields alist))

(define (update-test ht name values)
  (let ([ht-temp ht])
    (hashtable-set! ht-temp name values)
    ht-temp))

(define (update-test3 df name values)
  (let ([alist (data-frame-alist df)])
    

;; create-column
;; select/drop columns
;; rename columns
;; select rows (subset)
;; order rows (ascending and descending)
;; join rows
;; group by and summarize

;; https://stackoverflow.com/questions/23380385/how-do-i-get-the-sum-of-all-elements-10-in-a-given-list-using-chez-scheme


;; ;;  (system "curl https://raw.githubusercontent.com/pandas-dev/pandas/master/doc/data/tips.csv >> tips.csv")
;; (define tips (read-csv "tips.csv"))


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

;; (define (create-column ls name proc)
;; the only solution that I can come up with is to include a list of columns as an input
;; columns would need to be in same order as passed to lambda function
;; (create-column) would extract the columns map over them and add to the list
;; error prone in that requires user to specify correct order of columns in list and in lambda

;; (define a '((1 2 3) (4 5 6) (7 8 9)))

;; (define (create-column ls new-column columns proc)


;;    (map cons-end
;;       a
;;       (map (lambda (x) (+ (list-ref x 0) (list-ref x 1))) a))

;; ;;    (create-column a '() '(0 2) (lambda (x0 x2) (+ x0 x2)))

;; (define (adder x y)
;;   (+ x y))
