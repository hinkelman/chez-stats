(library (chez-stats dataframe)
  (export
   ->
   ->>
   $
   rowtable->dataframe
   dataframe->rowtable
   dataframe?
   ;dataframe-add
   dataframe-append
   dataframe-append-all
   dataframe-alist
   dataframe-contains?
   dataframe-dim
   dataframe-drop
   dataframe-equal?
   dataframe-filter
   dataframe-group-by
   dataframe-groups
   dataframe-head
   ;dataframe-map
   dataframe-names
   dataframe-names-update
   dataframe-partition
   dataframe-read
   dataframe-rename
   dataframe-select
   dataframe-tail
   dataframe-ungroup
   dataframe-unique
   ;dataframe-update
   dataframe-values
   dataframe-write
   make-dataframe)

  (import (chezscheme)
          (chez-stats assertions))

  ;; thread-first and thread-last -------------------------------------------------------------
  
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
      [(_ value) value]
      [(_ value (f1 . body) next ...)
       (->> (thread-last-helper f1 value . body) next ...)]))

  ;; handle expressions -------------------------------------------------------------
  ;; https://www.reddit.com/r/scheme/comments/e0lj08/lambda_eval_and_macros/
  
  ;; (define (handle-expr alist expr)
  ;;   (let* ([proc (car expr)]
  ;;          [args (cdr expr)])
  ;;     (apply map proc (map (lambda (x)
  ;;                         (handle-item alist x))
  ;;                       args))))

  ;; (define (handle-item alist item)
  ;;   (cond
  ;;    [(pair? item)
  ;;     (handle-expr alist item)]
  ;;    [(and (symbol? item) (assoc item alist))
  ;;     (cdr (assoc item alist))]
  ;;    [(or (number? item) (string? item) (symbol? item))
  ;;     (make-list (length (cdar alist)) item)]
  ;;    [else
  ;;     (assertion-violation "(handle-item alist item)" "expr is invalid")]))

  (define (handle-expr df expr who)
    (let* ([proc (car expr)]
           [args (cdr expr)])
      (apply map proc (map (lambda (x)
                          (handle-item df x who))
                        args))))

  (define (handle-item df item who)
    (cond
     [(pair? item)
      (handle-expr df item who)]
     [(and (symbol? item) (member item (dataframe-names df)))
      (dataframe-values df item)]
     [(or (number? item) (string? item) (symbol? item))
      (make-list (car (dataframe-dim df)) item)]
     [else
      (assertion-violation who "expr is invalid")]))

  (define (and-proc . args)
    (cond
     ((null? args) #t)
     ((not (car args)) #f)
     (else (apply and-proc (cdr args)))))

  (define (or-proc . args)
    (cond
     ((null? args) #t)
     ((not (car args)) #f)
     (else (apply or-proc (cdr args)))))

  (define (if-proc test consequent alternative)
    (if test consequent alternative))
  
  ;; dataframe record type ---------------------------------------------------------------------

  (define (drt-df-helper new alist groups)
    (let ([proc-string "(make-dataframe alist)"])
      (check-alist alist proc-string)
      (unless (null? groups)
        (check-alist groups proc-string)))
    (new alist
         groups
         (map car alist)
         (cons (length (cdar alist)) (length alist))))
  
  (define-record-type dataframe (fields alist groups names dim)
                      (protocol
                       (lambda (new)
                         (case-lambda
                           [(alist) (drt-df-helper new alist '())]
                           [(alist groups) (drt-df-helper new alist groups)]))))

  ;; grouped-df record type --------------------------------------------------------------------

  ;; no checking because grouped-df is internal only
  (define-record-type grouped-df (fields ls length)
                      (protocol
                       (lambda (new)
                         (lambda (ls)
                           (new ls (length ls))))))

  ;; check dataframes --------------------------------------------------------------------------
  
  (define (check-dataframe df who)
    (unless (dataframe? df)
      (assertion-violation who "df is not a dataframe")))

  (define (check-all-dataframes dfs who)
    (unless (for-all dataframe? dfs)
      (assertion-violation who "dfs are not all dataframes")))

  (define (dataframe-equal? . dfs)
    (check-all-dataframes dfs "(dataframe-equal? dfs)")
    (let* ([alists (map dataframe-alist dfs)]
           [first-alist (car alists)])
      (for-all (lambda (alist)
                 (equal? alist first-alist))
               alists)))

  ;; check dataframe attributes -------------------------------------------------------------
  
  (define (dataframe-contains? df . names)
    (check-dataframe df "(dataframe-contains? df names)")
    (let ([df-names (dataframe-names df)])
      (if (for-all (lambda (name) (member name df-names)) names) #t #f)))

  (define (check-names-exist df who . names)
    (unless (apply dataframe-contains? df names)
      (assertion-violation who "name(s) not in df")))

  (define (check-df-names df who . names)
    (if (grouped-df? df)
        (check-all-dataframes (grouped-df-ls df) who)
        (check-dataframe df who))
    (check-names names who)
    (apply check-names-exist df who names))

  ;; head/tail -----------------------------------------------------------------------------------

  (define (dataframe-head df n)
    (dataframe-head-tail df n "head"))

  ;; dataframe-tail is based on list-tail, which does not work the same as tail in R
  (define (dataframe-tail df n)
    (dataframe-head-tail df n "tail"))

  (define (dataframe-head-tail df n type)
    (let ([proc-string (string-append "(dataframe-" type " df n)")]
          [proc (if (string=? type "head") list-head list-tail)])
      (check-dataframe df proc-string)
      (check-positive-integer n "n" proc-string)
      (when (> n (car (dataframe-dim df)))
        (assertion-violation proc-string
                             (string-append "index " (number->string n) " is out of range")))
      (make-dataframe
       (map (lambda (col) (cons (car col) (proc (cdr col) n)))
            (dataframe-alist df)))))
  
  ;; rename columns ---------------------------------------------------------------------------------

  ;; name-pairs is of form '((old-name1 new-name1) (old-name2 new-name2))
  (define (dataframe-rename df name-pairs)
    (let ([proc-string "(dataframe df name-pairs)"])
      (check-dataframe df proc-string)
      (check-name-pairs (dataframe-names df) name-pairs proc-string))
    (let ([alist (map (lambda (column)
                        (let* ([name (car column)]
                               [ls-values (cdr column)]
                               [name-match (assoc name name-pairs)])
                          (if name-match
                              (cons (cadr name-match) ls-values)
                              column)))
                      (dataframe-alist df))])
      (make-dataframe alist)))

  (define (dataframe-names-update df new-names)
    (let ([proc-string "(dataframe df name-pairs)"])
      (check-dataframe df proc-string)
      (check-names new-names proc-string)
      (let ([new-names-length (length new-names)]
            [num-cols (cdr (dataframe-dim df))])
        (unless (= new-names-length num-cols)
          (assertion-violation proc-string (string-append
                                            "new-names length is "
                                            (number->string new-names-length)
                                            ", not "
                                            (number->string num-cols)))))
      (let* ([alist (dataframe-alist df)]
             [ls-col (map cdr alist)])
        (make-dataframe (add-names-ls-col new-names ls-col)))))

  ;; add names to list of columns, ls-col, to create association list
  (define (add-names-ls-col names ls-col)
    (if (null? ls-col)
        (map (lambda (name) (cons name '())) names)
        (map (lambda (name vals) (cons name vals)) names ls-col)))
  
  ;; append -----------------------------------------------------------------------------------

  (define (dataframe-append . dfs)
    (let ([proc-string "(dataframe-append dfs)"])
      (check-all-dataframes dfs proc-string)
      (let ([names (apply shared-names dfs)])
        (when (null? names) (assertion-violation proc-string "no names in common across dfs"))
        (let ([alist (map (lambda (name)
                            ;; missing-value will not be used so chose arbitrary value (-999)
                            (cons name (apply append-columns name -999 dfs)))
                          names)])
          (make-dataframe alist)))))
  
  (define (shared-names . dfs)
    (let ([first-names (dataframe-names (car dfs))]
          [rest-names (apply all-unique-names (cdr dfs))])
      (filter (lambda (name) (member name rest-names)) first-names)))

  (define (dataframe-append-all missing-value . dfs)
    (check-all-dataframes dfs "(dataframe-append-all missing-value dfs)")
    (let* ([names (apply combine-names-ordered dfs)]
           [alist (map (lambda (name)
                         (cons name (apply append-columns name missing-value dfs)))
                       names)])
      (make-dataframe alist)))

  (define (append-columns name missing-value . dfs)
    (apply append
           (map (lambda (df)
                  (if (dataframe-contains? df name)
                      (dataframe-values df name)
                      (make-list (car (dataframe-dim df)) missing-value))) 
                dfs)))

  (define (all-names . dfs)
    (apply append (map (lambda (df) (dataframe-names df)) dfs)))

  (define (all-unique-names . dfs)
    (remove-duplicates (apply all-names dfs)))

  ;; combine names such that they stay in the order that they appear in each dataframe
  (define (combine-names-ordered . dfs)
    (define (loop all-names results)
      (cond [(null? all-names)
             (reverse results)]
            [(member (car all-names) results)
             (loop (cdr all-names) results)]
            [else
             (loop (cdr all-names) (cons (car all-names) results))]))
    (loop (apply all-names dfs) '()))
  
  ;; thread-last works with dataframe-append
  ;; (->> (list df df) (apply dataframe-append))
  ;; can't use quote to create the list because then df becomes a symbol

  ;; read/write ------------------------------------------------------------------------------

  (define (dataframe-write df path overwrite?)
    (when (and (file-exists? path) (not overwrite?))
      (assertion-violation path "file already exists"))
    (delete-file path)
    (with-output-to-file path
      (lambda () (write (dataframe-alist df)))))

  (define (dataframe-read path)
    (make-dataframe (with-input-from-file path read)))

  ;; extract values ------------------------------------------------------------------------------

  ;; returns simple list
  (define (dataframe-values df name)
    (let ([proc-string "(dataframe-values df name)"])
      (check-dataframe df proc-string)
      (check-names-exist df proc-string name))
    (alist-values (dataframe-alist df) name))

  (define ($ df name)
    (dataframe-values df name))

  (define (alist-values alist name)
    (cdr (assoc name alist)))

  (define (dataframe-values-map df names)
    (alist-values-map (dataframe-alist df) names))

  (define (alist-values-map alist names)
    (map (lambda (name) (alist-values alist name)) names))

  ;; select/drop columns ------------------------------------------------------------------------

  (define (dataframe-select df . names)
    (apply check-df-names df "(dataframe-select df names)" names)
    (make-dataframe (alist-select (dataframe-alist df) names)))

  (define (alist-select alist names)
    (map (lambda (name) (assoc name alist)) names))

  (define (dataframe-drop df . names)
    (apply check-df-names df "(dataframe-drop df names)" names)
    (let ([alist (filter (lambda (column)
                           (not (member (car column) names)))
                         (dataframe-alist df))])
      (make-dataframe alist)))

  ;; update/add columns ------------------------------------------------------------------------

  ;; dataframe-add isn't very useful with group-by (and group-by part is not currently working)
  ;; because dataframe-add is only useful for combining multiple columns
  ;; b/c map is baked in it is too inflexible
  ;; perhaps create a function called dataframe-add that simply adds a column of values
  ;; however, dataframe-update has the same problem (of having map baked in)

  ;; (define (dataframe-add df new-name procedure names)
  ;;   (let ([proc-string "(dataframe-update df name procedure names)"])
  ;;     (check-new-names (dataframe-names df) '(new-name) proc-string)
  ;;     (check-procedure procedure proc-string))
  ;;   ;;  (check-df-names df names proc-string))
  ;;   (if (grouped-df? df)
  ;;       (apply dataframe-append (map (lambda (df-sub)
  ;;                                      (make-dataframe
  ;;                                       (alist-add
  ;;                                        (dataframe-alist df-sub)
  ;;                                        new-name
  ;;                                        procedure
  ;;                                        names)))
  ;;                                    df))
  ;;       (make-dataframe (alist-add (dataframe-alist df) new-name procedure names))))
    ;; (let ([ls-values (dataframe-values-map df names)])
    ;;   (make-dataframe (cons-end (dataframe-alist df)
    ;;                             (list new-name (apply map procedure ls-values))))))

  ;; (define dataframe-map
  ;;   (case-lambda
  ;;     [(df procedure names) (df-update-map df procedure names)]
  ;;     [(df procedure names new-name) (df-add-map df procedure names new-name)]))

  ;; (define (df-update-map df procedure names)
  ;;   (make-dataframe (alist-update-map (dataframe-alist df) procedure names)))
  
  ;; (define (df-add-map df procedure names new-name)
  ;;   (make-dataframe (alist-update-map (dataframe-alist df) procedure names new-name)))

  ;; (define (alist-update-map alist procedure ls-names)
  ;;   (map (lambda (column)
  ;;          (if (member (car column) ls-names)
  ;;              (list (car column) (map procedure (cdr column)))
  ;;              column))
  ;;        alist))

  ;; alist is original alist; alist-new is subset of alist, which has been updated
  (define (alist-update alist alist-new)
    (let ([names (map car alist-new)])
      (map (lambda (column)
             (let ([name (car column)])
               (if (member name names)
                   (assoc name alist-new)
                   column)))
           alist)))

  (define (alist-apply-map alist procedure names)
    (apply map procedure
           (alist-values-map alist names)))

  ;; (define alist-map
  ;;   (case-lambda
  ;;     [(alist procedure names)
  ;;      (alist-update alist (add-names-ls-col
  ;;                     names
  ;;                     (alist-apply-map alist procedure names)))]
  ;;     [(alist procedure names new-name)
  ;;      (cons-end alist (list new-name
  ;;                            (alist-apply-map alist procedure names new-name)))]))
                             
               
  ;; (define (alist-add-map alist procedure ls-names new-name)
  ;;   (cons-end alist
  ;;             (list new-name
  ;;                   (apply map procedure
  ;;                          (alist-values-map alist ls-names)))))

  (define (cons-end ls x)
    (reverse (cons x (reverse ls))))

  (define (dataframe-update df procedure . names)
    (let ([proc-string "(dataframe-update df procedure names)"])
      (check-procedure procedure proc-string)
      (apply check-df-names df proc-string names))
    (let ([alist (map (lambda (column)
                        (if (member (car column) names)
                            (list (car column) (map procedure (cdr column)))
                            column))
                      (dataframe-alist df))])
      (make-dataframe alist)))

  ;; filter/partition ------------------------------------------------------------------------

  (define (dataframe-partition df procedure . names)
    (let ([proc-string "(dataframe-partition df procedure names)"])
      (check-procedure procedure proc-string)
      (apply check-df-names df proc-string names))
    (let* ([ls-values (dataframe-values-map df names)]
           [ls-bool (apply map procedure ls-values)]
           [names (dataframe-names df)])
      (let-values ([(keep drop) (partition-ls-col ls-bool (map cdr (dataframe-alist df)))])
        (values (make-dataframe (add-names-ls-col names keep))
                (make-dataframe (add-names-ls-col names drop))))))

  ;; partition list of columns, ls-col, based on list of boolean values, ls-bool
  ;; where each sub-list is same length as ls-bool
  (define (partition-ls-col ls-bool ls-col)
    (let loop ([ls-bool ls-bool]
               [ls-col ls-col]
               [keep '()]
               [drop '()])
      (if (null? ls-bool)
          (values (map reverse keep)
                  (map reverse drop))
          (if (car ls-bool)  ;; ls is list of boolean values
              (loop (cdr ls-bool) (map cdr ls-col) (cons-acc ls-col keep) drop)
              (loop (cdr ls-bool) (map cdr ls-col) keep (cons-acc ls-col drop))))))

  ;; cons list of columns, ls-col, (usually length one) onto accumulator, acc
  (define (cons-acc ls-col acc)
    (if (null? acc)
        (map (lambda (x) (list (car x))) ls-col)
        (map (lambda (x y) (cons x y)) (map car ls-col) acc)))

  ;; (define (dataframe-filter df procedure . names)
  ;;   (let ([proc-string "(dataframe-filter df procedure names)"])
  ;;     (check-procedure procedure proc-string)
  ;;     (apply check-df-names df proc-string names))
  ;;   (let* ([ls-values (dataframe-values-map df names)]
  ;;          [ls-bool (apply map procedure ls-values)]
  ;;          [new-ls-col (filter-ls-col ls-bool (map cdr (dataframe-alist df)))]
  ;;          [names (dataframe-names df)])
  ;;     (make-dataframe (add-names-ls-col names new-ls-col))))

  ;; (define (dataframe-filter df procedure . names)
  ;;   (let ([proc-string "(dataframe-filter df procedure names)"])
  ;;     (check-procedure procedure proc-string)
  ;;     (apply check-df-names df proc-string names))
  ;;   (if (grouped-df? df)
  ;;       (make-grouped-df (map (lambda (df-sub) (make-dataframe (alist-filter (dataframe-alist df-sub) procedure names))) df))
  ;;       (make-dataframe (alist-filter (dataframe-alist df) procedure names))))


  (define (dataframe-filter df procedure . names)
    (let ([proc-string "(dataframe-filter df procedure names)"])
      (check-procedure procedure proc-string)
      (apply check-df-names df proc-string names))
    (if (grouped-df? df)
        (make-grouped-df (map (lambda (df-sub) (make-dataframe (df-filter-helper df-sub procedure names))) df))
        (make-dataframe (df-filter-helper df procedure names))))

  ;; lambda expressions capture environment at time that they are created
  ;; can't include unknown binding in lambda expression (i.e., needs to be in lambda arguments)
  ;; solution is maybe to require that dataframe (including grouped-df) is included in as lambda argument
  ;; but then need to figure out how to handle that b/c won't work with apply map procedure
  ;; pretty sure that I've circled back around to using macros
  (define (df-filter-helper df procedure names)
    (let* ([all-names (dataframe-names df)]
           [alist (dataframe-alist df)]
           [ls-values (alist-values-map alist names)]
           [ls-bool (apply map procedure ls-values)]
           [new-ls-col (filter-ls-col ls-bool (map cdr alist))])
      (add-names-ls-col all-names new-ls-col)))

  (define (alist-filter alist expr who)
    (let* ([all-names (map car alist)]
           [ls-bool (handle-expr alist expr who)]
           [new-ls-col (filter-ls-col ls-bool (map cdr alist))])
      (add-names-ls-col all-names new-ls-col)))

  ;; filter list of columns, ls-col, based on list of boolean values, ls-bool
  ;; where each sub-list is same length as ls-bool
  ;; could just call (partition-ls-col) and return only the first value
  ;; but avoiding potential overhead of accumulating values that aren't used
  (define (filter-ls-col ls-bool ls-col)
    (let loop ([ls-bool ls-bool]
               [ls-col ls-col]
               [results '()])
      (if (null? ls-bool)
          (map reverse results)
          (if (car ls-bool)
              (loop (cdr ls-bool) (map cdr ls-col) (cons-acc ls-col results))
              (loop (cdr ls-bool) (map cdr ls-col) results)))))

  (define (filter-ls-col2 ls-values ls-col procedure)
    (let ([ls-bool (apply map procedure ls-values)])
      (let loop ([ls-bool ls-bool]
                 [ls-col ls-col]
                 [results '()])
        (if (null? ls-bool)
            (map reverse results)
            (if (car ls-bool)
                (loop (cdr ls-bool) (map cdr ls-col) (cons-acc ls-col results))
                (loop (cdr ls-bool) (map cdr ls-col) results)))))
    (void))

  (define (filter-ls-col3 ls-col procedure)
    (let ([ls-rows (transpose ls-col)])
      (transpose (filter procedure ls-rows)))
    (void))

  (define (filter-ls-row ls-row procedure)
    (filter procedure ls-row)
    (void))

  ;; using built-in filter function is only slightly slower than recursive version
  ;; built-in version requires switching to row-wise and back to col-wise
  ;; i.e., it would be faster if dataframe use row-wise structure

  ;; select, mutate, aggregate are obviously better as column-wise
  ;; filter on a single column would be faster using row-wise
  ;; but want flexibility of filtering on multiple columns

  ;; don't know how to do column-wise sort without using indices, which would be slow and not idiomatic
  ;; not even sure how to execute column-wise sort with indices
  
  ;; simplest thing for multi-column sort is probably
  ;; (1) group
  ;; (2) transpose
  ;; (3) sort
  ;; (4) ungroup  ;; might be faster to transpose and then ungroup
  ;; (5) transpose

  ;; unique also uses transpose

  ;; not obvious (yet?) how to sort on multiple columns; already only able to sort on numeric characters
  ;; just need to push this forward and make a list of potential future features
    
  
  ;; unique ------------------------------------------------------------------------

  (define (dataframe-unique df)
    (check-dataframe df "(dataframe-unique df)")
    (make-dataframe (alist-unique (dataframe-alist df))))
  
  (define (alist-unique alist)
    (let ([names (map car alist)]
          [ls-col (map cdr alist)])
      (add-names-ls-col names (ls-col-unique ls-col))))

  (define (ls-col-unique ls-col)
    (transpose
     (remove-duplicates
      (transpose ls-col))))

  (define (transpose ls)
    (apply map list ls))

  ;; (define a (list (list 'trt (append (make-list 5 'A)
  ;;                                 (make-list 5 'B)))
  ;;              (list 'val (random-binomial 10 1 0.5))))
  ;; (alist-unique a)
  
  ;; (define b (list (list 'trt (append (make-list (inexact->exact 5e6) 'A)
  ;;                                 (make-list (inexact->exact 5e6) 'B)))
  ;;              (list 'val (random-poisson (inexact->exact 1e7) 10))))
  ;; (time (alist-unique b))

  ;; group-by ------------------------------------------------------------------------

  ;; if grouped; need to pass a separate lambda expression that will represent an outer map
  ;; only needed if doing something based on dataframe attributes
  ;; but needs to be incorporated in overall lambda expression
  ;; maybe use reserved word (e.g., DF) to refer to current df in lambda expression
  
  ;; returns boolean list of same length as list
  (define (contains obj ls)
    (let ([pred (cond
                 [(number? obj) =]
                 [(string? obj) string=?]
                 [(symbol? obj) symbol=?]
                 [else equal?])])
      (map (lambda (x) (pred obj x)) ls)))

  ;; list of objects to find in list of columns
  ;; not a good name but function is not exported
  (define (contains-andmap ls-obj ls-col)
    (let* ([ls-bool (map (lambda (obj ls)
                           (contains obj ls)) ls-obj ls-col)]
           [ls-row (transpose ls-bool)])
      (map (lambda (row)
             (for-all (lambda (x) (equal? x #t)) row))
           ls-row)))

  (define (group-by-helper ls-obj names-select alist)
    (let ([names (map car alist)]
          [ls-col (map cdr alist)]
          [ls-bool (contains-andmap
                    ls-obj
                    (map cdr (alist-select alist names-select)))])
      (let-values ([(keep drop) (partition-ls-col ls-bool ls-col)])
        (values (add-names-ls-col names keep)
                (add-names-ls-col names drop)))))
  
  (define (dataframe-group-by df names)
    (define (loop ls-row-unique alist results)
      (cond [(null? ls-row-unique)
             (make-grouped-df (reverse results))]
            [else
             (let-values ([(keep drop) (group-by-helper
                                        (car ls-row-unique)
                                        names
                                        alist)])
               (loop (cdr ls-row-unique)
                     drop
                     (cons (make-dataframe
                            keep
                            (add-names-ls-col names (transpose (list (car ls-row-unique)))))
                           results)))]))
    (apply check-df-names df "(dataframe-group-by df names)" names)
    (let* ([alist (dataframe-alist df)]
           [ls-col-select (map cdr (alist-select alist names))]
           [ls-row-unique (transpose (ls-col-unique ls-col-select))])
      (loop ls-row-unique alist '())))

  (define (dataframe-ungroup grouped-df)
    (unless (grouped-df? grouped-df)
      (assertion-violation "(dataframe-ungroup grouped-df)" "not a grouped df"))
    (apply dataframe-append (grouped-df-ls grouped-df)))

  ;; (define df (make-dataframe '((grp (a a b b b)) (trt (a b a b b)) (adult (1 2 3 4 5)) (juv (10 20 30 40 50)))))

  ;; group-by steps
  ;; create unique alist (also used as groups identifier in outer grouped dataframe); this won't work b/c groups could also be length one
  ;; get values for grouping columns as ls-col
  ;; loop through unique values (one for each ls-col)
  ;; map across groups for each ls-col and then map 'down' each column with (lambda (x) (equal? x unique-value))
  ;; transpose result
  ;; map for-all (equal? x #t) to get boolean list of same length as original dataframe
  ;; use boolean list to partition original alist
  ;; return 'keep' alist and pass 'drop' alist back to group-by

  ;; a grouped dataframe could be a dataframe where the alist contains the sub-dataframes
  ;; the overall dataframe would contain all groups in dataframe-groups
  ;; and each sub-dataframe would contain only groups for that sub-dataframe
  ;; solves the problem of what is passed to each function
  ;; and each function only needs to test if dataframe is grouped (rather than parsing whether it is a list of one etc)
  ;; inspired by list columns in R


  ;; rowtable ------------------------------------------------------------------------

  ;; bad name to describe list of rows; as used in read-csv and write-csv in (chez-stats csv)

  (define (dataframe->rowtable df)
    (check-dataframe df "(dataframe->rowtable df)")
    (let* ([names (dataframe-names df)]
           [ls-values (dataframe-values-map df names)])
      (cons names (transpose ls-values))))

  (define (rowtable->dataframe ls header?)
    (check-listtable ls "(rowtable->dataframe ls header?)")
    (let ([names (if header?
                     (car ls)
                     (map string->symbol
                          (map string-append
                               (make-list (length (car ls)) "V")
                               (map number->string
                                    (enumerate (car ls))))))]
          [ls-values (if header?
                         (transpose (cdr ls))
                         (transpose ls))])
      (make-dataframe (map cons names ls-values))))
  
  )


;; collecting a few misc things here for now
;; https://stackoverflow.com/questions/23380385/how-do-i-get-the-sum-of-all-elements-10-in-a-given-list-using-chez-scheme
;; (system "curl https://raw.githubusercontent.com/pandas-dev/pandas/master/doc/data/tips.csv >> tips.csv")
;; (define tips (read-csv "tips.csv"))
;; (system "gnuplot -e 'set terminal dumb; plot sin(x)'")


;; handle-expr approach has the pros of simplifying the expression syntax b/c not required to pass column names outside of expression
;; handle-expr approach has the downside of not using eager evaluation so an expression would always need to work on full column
;; handle-expr might not have same problems with lexical scope as I had with lambda but probably not worth the trouble given #2 on this list

;; probably should scale back the problem that I'm trying to solve to push this forward (b/c it has stalled out lately)
;; drop ability to make group-by work with mutate and filter
;; focus on group-by + aggregate (and work backwards?)





