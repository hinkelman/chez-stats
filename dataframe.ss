(library (chez-stats dataframe)
  (export
   ->
   dataframe->listtable
   dataframe?
   dataframe-add
   dataframe-append
   dataframe-append-all
   dataframe-alist
   dataframe-contains?
   dataframe-dim
   dataframe-drop
   dataframe-equal?
   dataframe-filter
   dataframe-groups
   dataframe-head
   dataframe-names
   dataframe-partition
   dataframe-read
   dataframe-rename
   dataframe-select
   dataframe-unique
   dataframe-update
   dataframe-values
   dataframe-write
   make-dataframe)

  (import (chezscheme)
          (chez-stats assertions))

  ;; ----------------------------- thread-first and thread-last --------------------------------
  
  ;; https://lispdreams.wordpress.com/2016/04/10/thread-first-thread-last-and-partials-oh-my/
  (define-syntax ->
    (syntax-rules ()
      [(_ value) value] 
      [(_ value (f1 . body) next ...)
       (-> (f1 value . body) next ...)]))

  (define (thread-last-helper f value . body)
    (apply f (append body (list value))))

  (define-syntax ->>
    (syntax-rules ()
      ((_ value) value)
      ((_ value (f1 . body) next ...)
       (->> (thread-last-helper f1 value . body) next ...))))

  ;; --------------------------- dataframe record type ------------------------------------------

  ;; still need to decide what groups should contain
  ;; e.g., list of group names or alist with group names and group levels
  ;; and then need to add a check that groups is not malformed to this procedure
  (define (drt-helper new alist groups)
    (check-alist alist "(make-dataframe alist)")
    (new alist
         groups
         (map car alist)
         (cons (length (cadar alist)) (length alist))))
  
  (define-record-type dataframe (fields alist groups names dim)
                      (protocol
                       (lambda (new)
                         (case-lambda
                           [(alist) (drt-helper new alist '())]
                           [(alist groups) (drt-helper new alist groups)]))))

  ;; --------------------------- check dataframes ------------------------------------------
  
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
                 (let ([ls-values (dataframe-values (car dfs) name)])
                   (for-all (lambda (df)
                              (and (member name (dataframe-names df))
                                   (equal? ls-values (dataframe-values df name))))
                            dfs)))
               names)))

  ;; --------------------------- check dataframe attributes ----------------------------------
  
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

  (define (check-df-names df names who)
    (check-dataframe df who)
    (check-names names who)
    (check-names-exist df names who))

  ;; --------------------------- head --------------------------------------------------------

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
  
  ;; --------------------------- rename columns --------------------------------------------------------

  (define (dataframe-rename df name-pairs)
    ;; name-pairs is of form '((old-name1 new-name1) (old-name2 new-name2))
    (let ([proc-string "(dataframe df name-pairs)"])
      (check-dataframe df proc-string)
      (check-name-pairs (dataframe-names df) name-pairs proc-string))
    (let ([alist (map (lambda (column)
                        (let* ([name (car column)]
                               [ls-values (cadr column)]
                               [name-match (assoc name name-pairs)])
                          (if name-match
                              (list (cadr name-match) ls-values)
                              column)))
                      (dataframe-alist df))])
      (make-dataframe alist)))
  
  ;; --------------------------- append --------------------------------------------------------

  (define (dataframe-append . dfs)
    (let ([proc-string "(dataframe-append dfs)"])
      (check-all-dataframes dfs proc-string)
      (let ([names (apply shared-names dfs)])
        (when (null? names) (assertion-violation proc-string "no names in common across dfs"))
        (let ([alist (map (lambda (name)
                            ;; missing-value will not be used so chose arbitrary value (-999)
                            (list name (apply append-columns name -999 dfs)))
                          names)])
          (make-dataframe alist)))))
  
  (define (shared-names . dfs)
    (let ([first-names (dataframe-names (car dfs))]
          [rest-names (apply combine-names (cdr dfs))])
      (filter (lambda (name) (member name rest-names)) first-names)))

  ;; simplifies life if dataframe-append only appends columns shared by all dfs
  ;; b/c wouldn't have to include missing-values argument in 
  ;; could rename this function to dataframe-append-all
  ;; dataframe-append would be used in split-apply-combine
  (define (dataframe-append-all missing-value . dfs)
    (check-all-dataframes dfs "(dataframe-append-all missing-value dfs)")
    (let* ([names (apply combine-names dfs)]
           [alist (map (lambda (name)
                         (list name (apply append-columns name missing-value dfs)))
                       names)])
      (make-dataframe alist)))

  (define (append-columns name missing-value . dfs)
    (apply append
           (map (lambda (df)
                  (if (dataframe-contains? df name)
                      (dataframe-values df name)
                      (make-list (car (dataframe-dim df)) missing-value))) 
                dfs)))

  (define (combine-names . dfs)
    (remove-duplicates
     (apply append
            (map (lambda (df) (dataframe-names df))
                 dfs))))

  ;; --------------------------- read/write ---------------------------------------------------

  (define (dataframe-write df path overwrite)
    (when (and (file-exists? path) (not overwrite))
      (assertion-violation path "file already exists"))
    (delete-file path)
    (with-output-to-file path
      (lambda () (write (dataframe-alist df)))))

  (define (dataframe-read path)
    (make-dataframe (with-input-from-file path read)))

  ;; --------------------------- extract values  ---------------------------------------------------

  ;; returns simple list
  (define (dataframe-values df name)
    (let ([proc-string "(dataframe-values df name)"])
      (check-dataframe df proc-string)
      (check-name-exists df name proc-string))
    (cadr (assoc name (dataframe-alist df))))

  ;; not exported
  (define (dataframe-values-map df . names)
    (map (lambda (name) (dataframe-values df name)) names))

  ;; --------------------------- select/drop columns  ---------------------------------------------

  (define (dataframe-select df . names)
    ;; select does not re-arrange columns, but not important because printing will not be effective
    ;; need to retain option to select and re-order columns from a list-table
    (let ([proc-string "(dataframe-select df names)"])
      (check-df-names df names proc-string))
    (let ([alist (filter (lambda (column)
                           (member (car column) names))
                         (dataframe-alist df))])
      (make-dataframe alist)))

  (define (dataframe-drop df . names)
    ;; select does not re-arrange columns, but not important because printing will not be effective
    ;; need to retain option to select and re-order columns from a list-table
    (let ([proc-string "(dataframe-drop df names)"])
      (check-df-names df names proc-string))
    (let ([alist (filter (lambda (column)
                           (not (member (car column) names)))
                         (dataframe-alist df))])
      (make-dataframe alist)))

  ;; --------------------------- update/add columns  ---------------------------------------------

  (define (dataframe-add df new-name procedure . names)
    (let ([proc-string "(dataframe-update df name procedure names)"])
      (check-new-names (dataframe-names df) '(new-name) proc-string)
      (check-procedure procedure proc-string)
      (check-df-names df names proc-string))
    (let ([ls-values (apply dataframe-values-map df names)])
      (make-dataframe (cons-end (dataframe-alist df)
                                (list new-name (apply map procedure ls-values))))))

  (define (cons-end ls x)
    (reverse (cons x (reverse ls))))

  (define (dataframe-update df procedure . names)
    (let ([proc-string "(dataframe-update df procedure names)"])
      (check-procedure procedure proc-string)
      (check-df-names df names proc-string))
    (let ([alist (map (lambda (column)
                        (if (member (car column) names)
                            (list (car column) (map procedure (cadr column)))
                            column))
                      (dataframe-alist df))])
      (make-dataframe alist)))

  ;; --------------------------- filter/partition  ---------------------------------------------

  (define (dataframe-partition df procedure . names)
    (let ([proc-string "(dataframe-partition df procedure names)"])
      (check-procedure procedure proc-string)
      (check-df-names df names proc-string))
    (let* ([ls-values (apply dataframe-values-map df names)]
           [bool-ls (apply map procedure ls-values)]
           [names (dataframe-names df)])
      (let-values ([(keep drop) (partition-ls-col bool-ls (map cadr (dataframe-alist df)))])
        (values (make-dataframe (add-names-ls-col names keep))
                (make-dataframe (add-names-ls-col names drop))))))

  ;; partition list of columns, ls-col, based on list of boolean values, ls
  ;; where each sub-list is same length as ls
  (define (partition-ls-col ls ls-col)
    (let loop ([ls ls]
               [ls-col ls-col]
               [keep '()]
               [drop '()])
      (if (null? ls)
          (values (map reverse keep)
                  (map reverse drop))
          (if (car ls)  ;; ls is list of boolean values
              (loop (cdr ls) (map cdr ls-col) (cons-acc ls-col keep) drop)
              (loop (cdr ls) (map cdr ls-col) keep (cons-acc ls-col drop))))))

  (define (cons-acc ls-col acc)
    (if (null? acc)
        (map (lambda (x) (list (car x))) ls-col)
        (map (lambda (x y) (cons x y)) (map car ls-col) acc)))

  (define (add-names-ls-col names ls-col)
    (if (null? ls-col)
        (map (lambda (name) (list name '())) names)
        (map (lambda (name vals) (list name vals)) names ls-col)))

  (define (dataframe-filter df procedure . names)
    (let ([proc-string "(dataframe-filter df procedure names)"])
      (check-procedure procedure proc-string)
      (check-df-names df names proc-string))
    (let* ([ls-values (apply dataframe-values-map df names)]
           [bool-ls (apply map procedure ls-values)]
           [new-ls-col (filter-ls-col bool-ls (map cadr (dataframe-alist df)))]
           [names (dataframe-names df)])
      (make-dataframe (add-names-ls-col names new-ls-col))))

  ;; filter list of columns, ls-col, based on list of boolean values, ls
  ;; where each sub-list is same length as ls
  ;; could just call (partition-ls-col) and return only the first value
  ;; but avoiding potential overhead of accumulating values that aren't used
  (define (filter-ls-col ls ls-col)
    (let loop ([ls ls]
               [ls-col ls-col]
               [results '()])
      (if (null? ls)
          (map reverse results)
          (if (car ls)
              (loop (cdr ls) (map cdr ls-col) (cons-acc ls-col results))
              (loop (cdr ls) (map cdr ls-col) results)))))
  
  
  ;; --------------------------- unique  ---------------------------------------------

  (define (dataframe-unique df)
    (check-dataframe df "(dataframe-unique df)")
    (make-dataframe (alist-unique (dataframe-alist df))))
  
  (define (alist-unique alist)
    (let* ([names (map car alist)]
           [ls-col (map cadr alist)]
           [ls-row (transpose ls-col)])
      (add-names-ls-col
       names
       (transpose
        (remove-duplicates ls-row)))))

  (define (transpose ls)
    (apply map list ls))



















  ;; ;; https://www.reddit.com/r/scheme/comments/e0lj08/lambda_eval_and_macros/
  ;; (define (handle-expr df expr who)
  ;;   (let* ([f (car expr)]
  ;;          [args (cdr expr)])
  ;;     (apply map f (map (lambda (x)
  ;;                         (handle-item df x who))
  ;;                       args))))

  ;; this fails if column contains symbols
  ;; could assume that if symbols aren't in df, then they should be expanded to length of column
  ;; (define (handle-item df item who)
  ;;   (cond
  ;;    [(pair? item) (handle-expr df item who)]
  ;;    [(symbol? item)
  ;;     (when (not (member item (dataframe-names df)))
  ;;       (assertion-violation who (string-append (symbol->string item) " is not a column in df")))
  ;;     (dataframe-values df item)]
  ;;    [(or (number? item) (string? item)) (make-list (car (dataframe-dim df)) item)]
  ;;    [else (assertion-violation who "expr is invalid")]))

  ;; (define (handle-item df item who)
  ;;   (cond
  ;;    [(pair? item)
  ;;     (handle-expr df item who)]
  ;;    [(and (symbol? item) (member item (dataframe-names df)))
  ;;     (dataframe-values df item)]
  ;;    [(or (number? item) (string? item) (symbol? item))
  ;;     (make-list (car (dataframe-dim df)) item)]
  ;;    [else
  ;;     (assertion-violation who "expr is invalid")]))


  






  (define (dataframe-group-by df . names)
    (let* ([df-unique (dataframe-unique (apply dataframe-select df names))])
      df-unique))

  ;; group-by steps
  ;; create unique alist (also used as groups identifier in outer grouped dataframe); this won't work b/c groups could also be length one
  ;; get values for grouping columns as ls-col
  ;; loop through unique values (one for each ls-col)
  ;; map across groups for each ls-col and then map 'down' each column with (lambda (x) (equal? x unique-value))
  ;; transpose result
  ;; map for-all (equal? x #t) to get boolean list of same length as original dataframe
  ;; use boolean list to partition original alist
  ;; return 'keep' alist and pass 'drop' alist back to group-by
  
  


  (define (listtable? ls)
    (and (list? ls)
         (for-all (lambda (row) (= (length row) (length (car ls)))) ls)))

  (define (listtable->dataframe ls header?)
    (map list
         (car ls)
         (apply map list (cdr ls)))) ;; transpose

  (define (dataframe->listtable df)
    (check-dataframe df "(dataframe->listable df)")
    (let* ([names (dataframe-names df)]
           [ls-values (apply dataframe-values-map df names)])
      (cons names (transpose ls-values))))
  
  ;; (define a (list (list 'trt (append (make-list 5 'A)
  ;;                                 (make-list 5 'B)))
  ;;              (list 'val (random-binomial 10 1 0.5))))
  ;; (alist-unique a)
  
  ;; (define b (list (list 'trt (append (make-list (inexact->exact 5e6) 'A)
  ;;                                 (make-list (inexact->exact 5e6) 'B)))
  ;;              (list 'val (random-poisson (inexact->exact 1e7) 10))))
  ;; (time (alist-unique b))
  

  ;; latest thinking on group-by...
  ;; dataframe-group-by should create a list of dataframes (using dataframe-partition)
  ;; maybe dataframe-partition needs optional groups argument for use in group-by
  ;; or perhaps just re-use small bit of overlapping code in dataframe-partition and dataframe-group-by
  ;; if you want to do the equivalent of group_by mutate in dplyr
  ;; then you will need to map over list of dataframes with dataframe-map
  ;; sidebar: this seems confusing, i.e., mapping the function called dataframe-map
  ;; and doesn't work with thread-first or thread-last
  ;; this mapping would then be followed by dataframe-append
  ;; which already removes grouping information from each dataframe
  ;; in this scenario, aggregate would only accept a list of dataframes
  ;; I'm now thinking that dataframe-map, dataframe-add, and dataframe-aggregate (and maybe dataframe-filter)
  ;; should all except either a list of dataframes or a single dataframe
  ;; and subsequent behavior depends on whether working with one or many dataframes
  ;; hopefully, this doesn't require too much additional checking of inputs
  ;; dataframe-aggregate is only function that will use information stored in dataframe-groups

  ;; a grouped dataframe could be a dataframe where the alist contains the sub-dataframes
  ;; the overall dataframe would contain all groups in dataframe-groups
  ;; and each sub-dataframe would contain only groups for that sub-dataframe
  ;; solves the problem of what is passed to each function
  ;; and each function only needs to test if dataframe is grouped (rather than parsing whether it is a list of one etc)
  ;; inspired by list columns in R

  ;; hash-table-map works like my current dataframe-update (except that procedure is applied to all keys)
  ;; hashtable-update! works like dataframe-update (except mutable) and only works on one key at a time

  ;; thread-last works with dataframe-append
  ;;  (->> (list df df) (apply dataframe-append -999))
  ;; can't use quote to create the list because then df becomes a symbol
  
  ;; (define (filter-wrap df name value)
  ;;   (dataframe-filter df `(,= ,name ,value)))

  ;; grouping columns must be strings or symbols
  ;; find unique combinations by pasting grouping variables together
  ;; loop through those unique combinations to partition dataframe
  ;; on 2nd thought, probably better to select columns
  ;; and then loop through to find unique combinations of rows

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







