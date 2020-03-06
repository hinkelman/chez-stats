(library (chez-stats dataframe)
  (export
   ->
   ->>
   $
   rowtable->dataframe
   dataframe->rowtable
   dataframe?
   ;;dataframe-add
   dataframe-append
   dataframe-append-all
   dataframe-alist
   dataframe-contains?
   dataframe-dim
   dataframe-drop
   dataframe-equal?
   dataframe-filter
   dataframe-head
   ;;dataframe-map
   dataframe-names
   dataframe-names-update
   dataframe-partition
   dataframe-read
   dataframe-rename
   dataframe-select
   dataframe-split
   dataframe-tail
   dataframe-unique
   ;;dataframe-update
   dataframe-values
   dataframe-write
   make-dataframe)

  (import (chezscheme)
          (chez-stats assertions))

  ;; naming conventions ----------------------------------------------------------------------

  ;; dataframe: a record-type comprised of an association list that meets specified criteria
  ;; alist: association list at the core of a dataframe; form is '((a 1 2 3) (b "this" "that" "other"))
  ;; alists: list of alists
  ;; column: one of the lists in the alist, includes name as first element
  ;; values: one of the lists in the alist, but with the name excluded
  ;; ls-values: values from an alist packaged into a list, e.g., '((1 2 3) ("this" "that" "other"))
  ;; name: column name
  ;; names: list of column names
  ;; group-names: list of column names used for grouping
  ;; df: single dataframe
  ;; dfs: list of dataframes
  ;; name-pairs: alist used in dataframe-rename; form is '((old-name1 new-name1) (old-name2 new-name2))
  ;; ls: generic list
  ;; x: generic object
  ;; procedure: lambda procedure
  ;; procedures: list of lambda procedures
  ;; bools: list of boolean values '(#t, #f)
  ;; row-based: describes orientation of list of lists; row-based example: '((a b) (1 "this") (2 "that") (3 "other))
  ;; rowtable: name used to describe a row-based list of lists where the first row represents column names
  ;; rt: single rowtable
  ;; ls-row: generic row-based list of lists
  
  

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

  ;; (define (handle-expr df expr who)
  ;;   (let* ([proc (car expr)]
  ;;          [args (cdr expr)])
  ;;     (apply map proc (map (lambda (x)
  ;;                         (handle-item df x who))
  ;;                       args))))

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

  ;; (define (and-proc . args)
  ;;   (cond
  ;;    ((null? args) #t)
  ;;    ((not (car args)) #f)
  ;;    (else (apply and-proc (cdr args)))))

  ;; (define (or-proc . args)
  ;;   (cond
  ;;    ((null? args) #t)
  ;;    ((not (car args)) #f)
  ;;    (else (apply or-proc (cdr args)))))

  ;; (define (if-proc test consequent alternative)
  ;;   (if test consequent alternative))
  
  ;; dataframe record type ---------------------------------------------------------------------

  (define-record-type dataframe (fields alist names dim)
                      (protocol
                       (lambda (new)
                         (lambda (alist)
                           (let ([proc-string "(make-dataframe alist)"])
                             (check-alist alist proc-string))
                           (new alist
                                (map car alist)
                                (cons (length (cdar alist)) (length alist)))))))

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
    (check-dataframe df who)
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

  (define (dataframe-names-update df names)
    (let ([proc-string "(dataframe-names-update df names)"])
      (check-dataframe df proc-string)
      (check-names names proc-string)
      (let ([names-length (length names)]
            [num-cols (cdr (dataframe-dim df))])
        (unless (= names-length num-cols)
          (assertion-violation proc-string (string-append
                                            "names length is "
                                            (number->string names-length)
                                            ", not "
                                            (number->string num-cols)))))
      (let* ([alist (dataframe-alist df)]
             [ls-values (map cdr alist)])
        (make-dataframe (add-names-ls-values names ls-values)))))

  ;; add names to list of values, ls-values, to create association list
  (define (add-names-ls-values names ls-values)
    (if (null? ls-values)
        (map (lambda (name) (cons name '())) names)
        (map (lambda (name vals) (cons name vals)) names ls-values)))
  
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
  ;;      (alist-update alist (add-names-ls-values
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

  (define (dataframe-partition df procedure)
    (let ([proc-string "(dataframe-partition df procedure)"])
      (check-procedure procedure proc-string)
      (check-dataframe df proc-string))
    (let* ([bools (procedure df)]
           [names (dataframe-names df)])
      (let-values ([(keep drop) (partition-ls-values bools (map cdr (dataframe-alist df)))])
        (values (make-dataframe (add-names-ls-values names keep))
                (make-dataframe (add-names-ls-values names drop))))))

  ;; partition list of values, ls-values, based on list of boolean values, bools
  ;; where each sub-list is same length as bools
  (define (partition-ls-values bools ls-values)
    (let loop ([bools bools]
               [ls-values ls-values]
               [keep '()]
               [drop '()])
      (if (null? bools)
          (values (map reverse keep)
                  (map reverse drop))
          (if (car bools)  ;; ls is list of boolean values
              (loop (cdr bools) (map cdr ls-values) (cons-acc ls-values keep) drop)
              (loop (cdr bools) (map cdr ls-values) keep (cons-acc ls-values drop))))))

  ;; cons list of values, ls-values, (usually length one) onto accumulator, acc
  (define (cons-acc ls-values acc)
    (if (null? acc)
        (map (lambda (x) (list (car x))) ls-values)
        (map (lambda (x y) (cons x y)) (map car ls-values) acc)))

  (define (dataframe-filter df procedure)
    (let ([proc-string "(dataframe-filter df procedure)"])
      (check-procedure procedure proc-string)
      (check-dataframe df proc-string))
    (make-dataframe (df-filter-helper df procedure)))

  (define (df-filter-helper df procedure)
    (let* ([all-names (dataframe-names df)]
           [alist (dataframe-alist df)]
           [bools (procedure df)]
           [new-ls-values (filter-ls-values bools (map cdr alist))])
      (add-names-ls-values all-names new-ls-values)))

  ;; filter list of values, ls-values, based on list of boolean values, bools
  ;; where each sub-list is same length as bools
  ;; could just call (partition-ls-values) and return only the first value
  ;; but avoiding potential overhead of accumulating values that aren't used
  (define (filter-ls-values bools ls-values)
    (let loop ([bools bools]
               [ls-values ls-values]
               [results '()])
      (if (null? bools)
          (map reverse results)
          (if (car bools)
              (loop (cdr bools) (map cdr ls-values) (cons-acc ls-values results))
              (loop (cdr bools) (map cdr ls-values) results)))))

  ;; in some simple and now deleted tests
  ;; using built-in filter function is only slightly slower than recursive version
  ;; built-in version requires switching to row-wise and back to col-wise (based on my current scheme abilities)
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
          [ls-values (map cdr alist)])
      (add-names-ls-values names (ls-values-unique ls-values #f))))

  (define (ls-values-unique ls-values row-based?)
    (let ([row-based (remove-duplicates (transpose ls-values))])
      (if row-based? row-based (transpose row-based))))

  (define (transpose ls)
    (apply map list ls))


  ;; split ------------------------------------------------------------------------

  ;; returns boolean list of same length as ls
  ;; boolean list used to identify rows that are equal to focal value, x
  (define (map-equal? x ls)
    (let ([pred (cond
                 [(number? x) =]
                 [(string? x) string=?]
                 [(symbol? x) symbol=?]
                 [else equal?])])
      (map (lambda (y) (pred x y)) ls)))

  ;; andmap-equal? probably not a good name
  ;; objective is to identify rows from ls-values where every row matches target values in ls
  (define (andmap-equal? ls ls-values)
    (let* ([bools (map (lambda (x values)
                         (map-equal? x values))
                       ls
                       ls-values)]
           [ls-row (transpose bools)])
      (map (lambda (row)
             (for-all (lambda (x) (equal? x #t)) row))
           ls-row)))

  (define (alist-split-helper ls group-names alist)
    (let ([names (map car alist)]
          [ls-values (map cdr alist)]
          [bools (andmap-equal?
                  ls
                  (map cdr (alist-select alist group-names)))])
      (let-values ([(keep drop) (partition-ls-values bools ls-values)])
        (values (add-names-ls-values names keep)
                (add-names-ls-values names drop)))))
  
  (define (alist-split alist group-names)
    (define (loop ls-row-unique alist alists groups)
      (cond [(null? ls-row-unique)
             (values (reverse alists)
                     (reverse groups))]
            [else
             (let ([group-values (car ls-row-unique)]) ; single row of values representing one unique grouping combination
               (let-values ([(keep drop) (alist-split-helper group-values group-names alist)])
                 (loop (cdr ls-row-unique)
                       drop
                       (cons keep alists)
                       (cons (add-names-ls-values group-names (transpose (list group-values))) groups))))])) 
    (let* ([ls-values-select (map cdr (alist-select alist group-names))]
           [ls-row-unique (ls-values-unique ls-values-select #t)])
      (loop ls-row-unique alist '() '())))

  (define (dataframe-split-helper df group-names return-groups?)
    (apply check-df-names df "(dataframe-split df group-names)" group-names)
    (let-values ([(alists groups) (alist-split (dataframe-alist df) group-names)])
      (let ([dfs (map make-dataframe alists)])
        (if return-groups?
            (values dfs groups)
            dfs))))

  (define dataframe-split
    (case-lambda
      [(df group-names) (dataframe-split-helper df group-names #f)]
      [(df group-names return-groups?) (dataframe-split-helper df group-names return-groups?)]))
  
  ;; aggregate  ------------------------------------------------------------------------

  ;; can't use alist-aggregate because user will be passing dataframe specific procedures
  
  (define (alist-aggregate alist group-names procs new-names)
    (let-values ([(groups alists) (alist-split alist group-names)])
      (map (lambda (alist) (map (lambda (proc) (proc alist)) procs)) alists)))
  
  ;; rowtable ------------------------------------------------------------------------

  ;; rowtable is a bad name to describe list of rows; as used in read-csv and write-csv in (chez-stats csv)

  (define (dataframe->rowtable df)
    (check-dataframe df "(dataframe->rowtable df)")
    (let* ([names (dataframe-names df)]
           [ls-values (dataframe-values-map df names)])
      (cons names (transpose ls-values))))

  (define (rowtable->dataframe rt header?)
    (check-rowtable rt "(rowtable->dataframe rt header?)")
    (let ([names (if header?
                     (car rt)
                     (map string->symbol
                          (map string-append
                               (make-list (length (car rt)) "V")
                               (map number->string
                                    (enumerate (car rt))))))]
          [ls-values (if header?
                         (transpose (cdr rt))
                         (transpose rt))])
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





