(library (chez-stats dataframe)
  (export
   ->
   ->>
   listtable->dataframe
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
   dataframe-tail
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

  (define (drt-helper new alist groups)
    (let ([proc-string "(make-dataframe alist)"])
      (check-alist alist proc-string)
      (unless (null? groups)
        (check-alist groups proc-string)))
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

  ;; comparing at column level because column order doesn't matter for equality of dataframes
  (define (dataframe-equal? . dfs)
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

  ;; --------------------------- head/tail --------------------------------------------------------

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

  ;; dataframe-tail is not same as list-tail
  ;; instead, works more like tail in R
  (define (dataframe-tail df n)
    (let ([proc-string "(dataframe-tail df n)"])
      (check-dataframe df proc-string)
      (check-positive-integer n "n" proc-string)
      (let ([rows (car (dataframe-dim df))])
        (when (> n rows)
          (assertion-violation proc-string
                               (string-append "index " (number->string n) " is out of range")))
        (make-dataframe
         (map (lambda (col) (list (car col) (list-tail (cadr col) (- rows n))))
              (dataframe-alist df))))))
  
  ;; --------------------------- rename columns ------------------------------------------------------

  ;; name-pairs is of form '((old-name1 new-name1) (old-name2 new-name2))
  (define (dataframe-rename df name-pairs)
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

  ;; thread-last works with dataframe-append
  ;; (->> (list df df) (apply dataframe-append))
  ;; can't use quote to create the list because then df becomes a symbol

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
    (let ([proc-string "(dataframe-select df names)"])
      (check-df-names df names proc-string))
    (let ([alist (filter (lambda (column)
                           (member (car column) names))
                         (dataframe-alist df))])
      (make-dataframe alist)))

  (define (dataframe-drop df . names)
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
           [ls-bool (apply map procedure ls-values)]
           [names (dataframe-names df)])
      (let-values ([(keep drop) (partition-ls-col ls-bool (map cadr (dataframe-alist df)))])
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

  ;; add names to list of columsn, ls-col, to create association list
  (define (add-names-ls-col names ls-col)
    (if (null? ls-col)
        (map (lambda (name) (list name '())) names)
        (map (lambda (name vals) (list name vals)) names ls-col)))

  (define (dataframe-filter df procedure . names)
    (let ([proc-string "(dataframe-filter df procedure names)"])
      (check-procedure procedure proc-string)
      (check-df-names df names proc-string))
    (let* ([ls-values (apply dataframe-values-map df names)]
           [ls-bool (apply map procedure ls-values)]
           [new-ls-col (filter-ls-col ls-bool (map cadr (dataframe-alist df)))]
           [names (dataframe-names df)])
      (make-dataframe (add-names-ls-col names new-ls-col))))

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
  
  ;; --------------------------- unique ---------------------------------------------

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

  ;; (define a (list (list 'trt (append (make-list 5 'A)
  ;;                                 (make-list 5 'B)))
  ;;              (list 'val (random-binomial 10 1 0.5))))
  ;; (alist-unique a)
  
  ;; (define b (list (list 'trt (append (make-list (inexact->exact 5e6) 'A)
  ;;                                 (make-list (inexact->exact 5e6) 'B)))
  ;;              (list 'val (random-poisson (inexact->exact 1e7) 10))))
  ;; (time (alist-unique b))

  ;; --------------------------- group-by ---------------------------------------------

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

  ;; a grouped dataframe could be a dataframe where the alist contains the sub-dataframes
  ;; the overall dataframe would contain all groups in dataframe-groups
  ;; and each sub-dataframe would contain only groups for that sub-dataframe
  ;; solves the problem of what is passed to each function
  ;; and each function only needs to test if dataframe is grouped (rather than parsing whether it is a list of one etc)
  ;; inspired by list columns in R


  ;; --------------------------- listtable ---------------------------------------------

  ;; bad name to describe list of rows; as used in read-csv and write-csv

  (define (dataframe->listtable df)
    (check-dataframe df "(dataframe->listable df)")
    (let* ([names (dataframe-names df)]
           [ls-values (apply dataframe-values-map df names)])
      (cons names (transpose ls-values))))

  (define (listtable->dataframe ls header?)
    (check-listtable ls "(listtable->dataframe ls header?)")
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
      (make-dataframe (map list names ls-values))))
      
  )


;; collecting a few misc things here for now
;; https://stackoverflow.com/questions/23380385/how-do-i-get-the-sum-of-all-elements-10-in-a-given-list-using-chez-scheme
;; (system "curl https://raw.githubusercontent.com/pandas-dev/pandas/master/doc/data/tips.csv >> tips.csv")
;; (define tips (read-csv "tips.csv"))
;; (system "gnuplot -e 'set terminal dumb; plot sin(x)'")


;; ;; https://www.reddit.com/r/scheme/comments/e0lj08/lambda_eval_and_macros/
;; (define (handle-expr df expr who)
;;   (let* ([f (car expr)]
;;          [args (cdr expr)])
;;     (apply map f (map (lambda (x)
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





