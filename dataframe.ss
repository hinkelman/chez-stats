(library (chez-stats dataframe)
  (export ->
	  and2
	  or2
	  dataframe?
	  dataframe-add
	  dataframe-append
	  dataframe-alist
	  dataframe-contains?
	  dataframe-dim
	  dataframe-drop
	  dataframe-equal?
	  dataframe-filter
	  dataframe-head
	  dataframe-map
	  dataframe-names
	  dataframe-rename
	  dataframe-select
	  dataframe-update
	  dataframe-values
	  make-dataframe)

  (import (chezscheme)
	  (chez-stats assertions))

  ;; https://lispdreams.wordpress.com/2016/04/10/thread-first-thread-last-and-partials-oh-my/
  (define-syntax ->
    (syntax-rules ()
      ((_ value) value)
      ((_ value (f1 . body) next ...)
       (-> (f1 value . body) next ...))))
  
  (define-record-type dataframe (fields alist names dim)
		      (protocol
		       (lambda (new)
			 (lambda (alist)
			   (check-alist alist "(make-dataframe alist)")
			   (new alist
				(map car alist)
				(cons (length (cadar alist)) (length alist)))))))
  
  (define (check-dataframe df who)
    (unless (dataframe? df)
      (assertion-violation who "df is not a dataframe")))

  (define (check-all-dataframes dfs who)
    (unless (for-all dataframe? dfs)
      (assertion-violation who "dfs are not all dataframes")))

  (define (dataframe-equal? . dfs)
    ;; comparing at column level because column order doesn't matter for equality of dataframes
    (check-all-dataframes dfs "(dataframe-equal? dfs)")
    (let ([names (dataframe-names (car dfs))])
      (for-all (lambda (name)
		 (let ([values (dataframe-values (car dfs) name)])
		   (for-all (lambda (df)
			      (and (member name (dataframe-names df))
				   (equal? values (dataframe-values df name))))
			    dfs)))
	       names)))
  
  (define (dataframe-contains? df . names)
    (check-dataframe df "(dataframe-contains? df name)")
    (let ([df-names (dataframe-names df)])
      (if (for-all (lambda (name) (member name df-names)) names) #t #f)))

  (define (check-names-exist df names who)
    (unless (apply dataframe-contains? df names)
      (assertion-violation who "at least one name not in df")))

  (define (check-name-exists df name who)
    (unless (dataframe-contains? df name)
      (assertion-violation who "name not in df")))

  (define (dataframe-head df n)
    (let ([proc-string "(dataframe-head df n)"])
      (check-dataframe df proc-string)
      (check-positive-integer n "n" proc-string)
      (when (> n (car (dataframe-dim df)))
	(assertion-violation proc-string
			     (string-append "index " (number->string n) " is out of range")))
      (make-dataframe
       (map (lambda (col) (list (car col) (list-head (cadr col) n)))
	    (dataframe-alist df)))))

  (define (dataframe-values df name)
    (let ([proc-string "(dataframe-values df name)"])
      (check-dataframe df proc-string)
      (check-name-exists df name proc-string))
    (cadr (assoc name (dataframe-alist df))))

  (define (dataframe-append missing-value . dfs)
    (check-all-dataframes dfs "(dataframe-append missing-value dfs)")
    (let* ([names (remove-duplicates
		   (apply append (map
				  (lambda (df) (dataframe-names df)) dfs)))]
	   [alist (map
		   (lambda (name)
		     (list name (apply append
				       (map
					(lambda (df)
					  (if (dataframe-contains? df name)
					      (dataframe-values df name)
					      (make-list (car (dataframe-dim df)) missing-value))) 
					dfs))))
		   names)])
      (make-dataframe alist)))

  (define (dataframe-rename df name-pairs)
    ;; name-pairs is of form '((old-name1 new-name1) (old-name2 new-name2))
    (let ([proc-string "(dataframe df name-pairs)"])
      (check-dataframe df proc-string)
      (check-name-pairs (dataframe-names df) name-pairs proc-string))
    (let ([alist (map (lambda (column)
			(let* ([name (car column)]
			       [values (cadr column)]
			       [name-match (assoc name name-pairs)])
			  (if name-match
			      (list (cadr name-match) values)
			      column)))
		      (dataframe-alist df))])
      (make-dataframe alist)))

  (define (dataframe-select df . names)
    ;; select does not re-arrange columns, but not important because printing will not be effective
    ;; need to retain option to select and re-order columns from a list-table
    (let ([proc-string "(dataframe-select df names)"])
      (check-dataframe df proc-string)
      (check-names names proc-string)
      (check-names-exist df names proc-string))
    (let ([alist (filter (lambda (column)
			   (member (car column) names))
			 (dataframe-alist df))])
      (make-dataframe alist)))

  (define (dataframe-drop df . names)
    ;; select does not re-arrange columns, but not important because printing will not be effective
    ;; need to retain option to select and re-order columns from a list-table
    (let ([proc-string "(dataframe-drop df names)"])
      (check-dataframe df proc-string)
      (check-names names proc-string)
      (check-names-exist df names proc-string))
    (let ([alist (filter (lambda (column)
			   (not (member (car column) names)))
			 (dataframe-alist df))])
      (make-dataframe alist)))

  (define (dataframe-update df procedure . names)
    (let ([proc-string "(dataframe-update df procedure names)"])
      (check-dataframe df proc-string)
      (check-procedure procedure proc-string)
      (check-names names proc-string)
      (check-names-exist df names proc-string))
    (let ([alist (map (lambda (column)
			(if (member (car column) names)
			    (list (car column) (map procedure (cadr column)))
			    column))
		      (dataframe-alist df))])
      (make-dataframe alist)))

  (define (cons-end ls x)
    (reverse (cons x (reverse ls))))
  
  (define (dataframe-add df name values)
    (let ([proc-string "(dataframe df name values)"])
      (check-dataframe df proc-string)
      (check-new-names (dataframe-names df) '(name) proc-string)
      (check-values (car (dataframe-dim df)) values proc-string))
    (make-dataframe (cons-end (dataframe-alist df) (list name values))))

  ;; https://www.reddit.com/r/scheme/comments/e0lj08/lambda_eval_and_macros/
  (define (handle-expr df expr who)
    (let* ([f (car expr)]
	   [args (cdr expr)])
      (apply map f (map (lambda (x)
			  (handle-item df x who))
			args))))
    
  (define (handle-item df item who)
    (cond
     [(pair? item) (handle-expr df item who)]
     [(symbol? item)
      (when (not (member item (dataframe-names df)))
	(assertion-violation who (string-append (symbol->string item) " is not a column in df")))
      (dataframe-values df item)]
     [(or (number? item) (string? item)) (make-list (car (dataframe-dim df)) item)]
     [else (assertion-violation who "expr is invalid")]))
  
  (define (dataframe-map df name expr)
    (let ([proc-string "(dataframe-map df name expr)"])
      (check-dataframe df proc-string)
      (check-new-names (dataframe-names df) '(name) proc-string)
      (dataframe-add df name (handle-expr df expr proc-string))))

  ;; ;; filter two lists where ls is list filtered by procedure and
  ;; ;; lol is list of lists where each sub-list is same length as ls
  ;; (define (filter-two procedure ls lol)
  ;;   (let loop ([ls ls]
  ;; 	       [lol lol]
  ;; 	       [res-ls '()]
  ;; 	       [res-lol '()])
  ;;     (cond [(null? ls)
  ;; 	     (values (reverse res-ls)
  ;; 		     (map reverse res-lol))]
  ;; 	    [(procedure (car ls))
  ;; 	     (loop (cdr ls)
  ;; 		   (map cdr lol)
  ;; 		   (cons (car ls) res-ls)
  ;; 		   (if (null? res-lol)
  ;; 		       (map (lambda (x) (list (car x))) lol)
  ;; 		       (map (lambda (x y) (cons x y)) (map car lol) res-lol)))]
  ;; 	    [else
  ;; 	     (loop (cdr ls) (map cdr lol) res-ls res-lol)])))

  ;; filter list of lists, lol, based on list of boolean values, ls
  ;; where each sub-list is same length as ls
  (define (filter-lol ls lol)
    (let loop ([ls ls]
	       [lol lol]
	       [results '()])
      (if (null? ls)
	  (map reverse results)
	  (if (car ls)
	      (loop (cdr ls)
		    (map cdr lol)
		    (if (null? results)
			(map (lambda (x) (list (car x))) lol)
			(map (lambda (x y) (cons x y)) (map car lol) results)))
	      (loop (cdr ls) (map cdr lol) results)))))
		 
  (define (and2 expr1 expr2)
    (and expr1 expr2))

  (define (or2 expr1 expr2)
    (or expr1 expr2))
 
  ;; (define (dataframe-filter df name procedure)
  ;;   (let ([proc-string "(dataframe-filter df name procedure)"])
  ;;     (check-dataframe df proc-string)
  ;;     (check-procedure procedure proc-string)
  ;;     (check-name-exists df name proc-string))
  ;;   (let ([ls (dataframe-values df name)]
  ;; 	  [other-names (filter (lambda (x) (not (symbol=? x name)))
  ;; 			       (dataframe-names df))])
  ;;     (if (= 1 (length (dataframe-names df)))
  ;; 	  (make-dataframe (list (list name (filter procedure ls))))
  ;; 	  (let-values ([(new-ls new-lol)
  ;; 			(filter-two procedure ls
  ;; 				    (map cadr (dataframe-alist
  ;; 					       (dataframe-drop df name))))])
  ;; 	    (dataframe-add (make-dataframe
  ;; 			    (map (lambda (x y) (list x y)) other-names new-lol))
  ;; 			   name
  ;; 			   new-ls)))))

  (define (dataframe-filter df expr)
    (let ([proc-string "(dataframe-filter df expr)"])
      (check-dataframe df proc-string)
      (let* ([bool-ls (handle-expr df expr proc-string)]
	     [new-lol (filter-lol bool-ls (map cadr (dataframe-alist df)))]
	     [names (dataframe-names df)])
	(if (null? new-lol)
	    (make-dataframe (map (lambda (name) (list name '())) names))
	    (make-dataframe (map (lambda (name values) (list name values)) names new-lol))))))
	
    











  



  ;; (define (listtable? ls)
  ;;   (and (list? ls)
  ;; 	 (for-all (lambda (row) (= (length row) (length (car ls)))) ls)))

  ;; (define (listtable->dataframe ls header?)
  ;;   (map list
  ;; 	 (car ls)
  ;; 	 (apply map list (cdr ls)))) ;; transpose

  ;; (define (dataframe->listtable df)
  ;;   (cons (map car df)
  ;; 	  (apply map list (map cadr df))))


  )


;; create-column
;; select/drop columns
;; rename columns
;; select rows; dataframe-filter; based on values in a single column, based on values in other columns, and based on row indices
;; order rows (ascending and descending)
;; join rows
;; group by and summarize

;; https://stackoverflow.com/questions/23380385/how-do-i-get-the-sum-of-all-elements-10-in-a-given-list-using-chez-scheme

;; (system "curl https://raw.githubusercontent.com/pandas-dev/pandas/master/doc/data/tips.csv >> tips.csv")
;; (define tips (read-csv "tips.csv"))

;; (system "gnuplot -e 'set terminal dumb; plot sin(x)'")

;; ;; core logic for sorting list of lists
;; ;; split apply combine to sort on multiple dimensions
;; ;; need to figure out filtering first
;; (sort (lambda (x y)(< (car x)(car y))) d)

;; ;; core logic for filtering on one column
;; (filter (lambda (x) (> (list-ref x 2) 5)) a)







