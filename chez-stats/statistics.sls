(library (chez-stats statistics)
  (export
   count-unique
   correlation
   cumulative-sum
   diff
   interquartile-range
   kurtosis
   mean
   median
   mode
   quantile
   rank
   rep
   rle
   sign
   skewness
   standard-deviation
   sum
   unique
   variance
   weighted-mean)

  (import (chezscheme)
	  (chez-stats assertions))

  (define (sum lst)
    (let ([proc-string "(sum lst)"])
      (check-is-list lst "lst" proc-string)
      (check-empty-list lst "lst" proc-string)
      (cond
       [(for-all real? lst) (apply + lst)]
       [(for-all boolean? lst) (length (filter (lambda (x) x) lst))]
       [else (assertion-violation
              proc-string
              "all elements of lst need to be either real numbers or boolean values")])))
              
  (define (rle lst)
    ;; run length encoding
    ;; returns a list of pairs where the car and cdr of each pair
    ;; are the values and lengths of the runs, respectively, for the values in lst
    (check-list lst "lst" "(rle lst)")
    (let loop ([first (car lst)]
               [rest (cdr lst)]
               [n 1]
               [vals '()]
               [counts '()])
      (cond
       [(null? rest)
	(map cons
	     (reverse (cons first vals))
	     (reverse (cons n counts)))]
       [(= first (car rest))
	(loop (car rest) (cdr rest) (add1 n) vals counts)]
       [else
	(loop (car rest) (cdr rest) 1 (cons first vals) (cons n counts))])))
  
  (define (count-unique lst)
    (check-list lst "lst" "(count-unique lst)")
    (rle (sort < lst)))
  
  (define (mode lst)
    ;; returns a list with the value(s) in lst that occur most frequently
    (check-list lst "lst" "(mode lst)")
    (let* ([val-count (count-unique lst)]
	   [mx (apply max (map cdr val-count))])
      (filter (lambda (x) (not (null? x)))
	      (map (lambda (val count) (if (= count mx) val '()))
		   (map car val-count)
		   (map cdr val-count)))))

  (define (unique lst)
    (check-list lst "lst" "(unique lst)")
    (map car (count-unique lst)))

  (define interquartile-range
    ;; type is an integer from 1-9
    (case-lambda
      [(lst) (iqr lst 8)]
      [(lst type) (iqr lst type)]))

  (define (iqr lst type)
    (let ([proc-string "(interquartile-range lst type)"])
      (check-list lst "lst" proc-string)
      (check-quantile-type type proc-string))
    (let ([lwr (quantile lst 0.25 type)]
          [upr (quantile lst 0.75 type)])
      (- upr lwr)))

  (define quantile
    ;; type is an integer from 1-9
    (case-lambda
      [(lst p) (quantile-helper lst p 8)]
      [(lst p type) (quantile-helper lst p type)]))

  (define (quantile-helper lst p type)
    ;; see https://www.jstor.org/stable/2684934 for quantile-helper, calc-Q, and get-gamma
    (let ([proc-string "(quantile lst p type)"])
      (check-list lst "lst" proc-string)
      (check-p p proc-string)
      (check-quantile-type type proc-string))
    (let* ([n (length lst)]
           [order-stats (sort < lst)]
	   ;; ms is list of m values for each quantile type
	   [ms (list 0 0 -1/2 0 1/2 p (- 1 p) (* (add1 p) 1/3) (+ (* p 1/4) 3/8))]
	   [m (list-ref ms (sub1 type))]
	   [j-tmp (floor (+ (* n p) m))]
	   ;; j needs to be a fixnum for indexing
	   [j (if (flonum? j-tmp) (flonum->fixnum j-tmp) j-tmp)]
	   [g (- (+ (* n p) m) j)]
	   [gamma (get-gamma g j type)])
      (calc-Q order-stats j gamma)))

  (define (calc-Q order-stats j gamma)
    ;; j is described for one-based indexing; adjusted for zero-based indexing
    (let ([n-os (length order-stats)])
      (cond
       [(< j 1) (list-ref order-stats 0)]
       [(>= j n-os) (list-ref order-stats (sub1 n-os))]
       [else (+ (* (- 1 gamma) (list-ref order-stats (sub1 j)))
		(* gamma (list-ref order-stats j)))])))
  
  (define (get-gamma g j type)
    (cond
     [(= type 1) (if (= g 0) 0 1)]
     [(= type 2) (if (= g 0) 0.5 1)]
     [(= type 3) (if (and (= g 0) (even? j)) 0 1)]
     [else g]))

  (define (median lst)
    ;; type 7 is chosen to match R even though default for quantile in chez-stats is 8
    (check-list lst "lst" "(median lst)")
    (quantile lst 0.5 7))

  (define (cumulative-sum lst)
    (check-list lst "lst" "(cumulative-sum lst)")
    (let loop ([lst lst]
	       [result '()]
	       [total 0])
      (if (null? lst)
	  (reverse result)
	  (let ([new-total (+ (car lst) total)])
	    (loop (cdr lst) (cons new-total result) new-total)))))
  
  (define (mean lst)
    (check-list lst "lst" "(mean lst)")
    (/ (apply + lst) (length lst)))

  (define (skewness lst)
    ;; based on skewness function from R package `moments`
    (check-list lst "lst" "(skewness lst)")
    (let* ([n (length lst)]
	   [x-bar (mean lst)]
	   [x-diff (map (lambda (x) (- x x-bar)) lst)]
	   [num (/ (apply + (map (lambda (x) (expt x 3)) x-diff)) n)]
	   [den (/ (apply + (map (lambda (x) (expt x 2)) x-diff)) n)])
      (/ num (expt den (/ 3 2)))))

  (define (kurtosis lst)
    ;; based on kurtosis function from R package `moments`
    (check-list lst "lst" "(kurtosis lst)")
    (let* ([n (length lst)]
	   [x-bar (mean lst)]
	   [x-diff (map (lambda (x) (- x x-bar)) lst)]
	   [num (apply + (map (lambda (x) (expt x 4)) x-diff))]
	   [den (expt (apply + (map (lambda (x) (expt x 2)) x-diff)) 2)])
      (* n (/ num den))))

  (define (weighted-mean lst weights)
    (let ([proc-string "(weighted-mean lst weights)"])
      (check-list lst "lst" proc-string )
      (check-list weights "weights" proc-string)
      (unless (= (length lst) (length weights))
	(assertion-violation proc-string "lst and weights are not the same length")))
    (/ (apply + (map (lambda (x y) (* x y)) lst weights)) (apply + weights)))

  (define (variance lst)
    (check-list lst "lst" "(variance lst)")
    (variance-helper lst))

  (define (variance-helper lst)
    ;; https://www.johndcook.com/blog/standard_deviation/
    (define (update-ms x ms i)
      ;; ms is a pair of m and s variables
      ;; x is current value of lst in loop
      (let* ([m (car ms)]
	     [s (cdr ms)]
	     [new-m (+ m (/ (- x m) (add1 i)))]
             [new-s (+ s (* (- x m) (- x new-m)))])
	(cons new-m new-s)))
    (let loop ([lst (cdr lst)]
	       [ms (cons (car lst) 0)] 
	       [i 1])                 ; one-based indexing in the algorithm
      (if (null? lst)
	  (/ (cdr ms) (- i 1))
          (loop (cdr lst) (update-ms (car lst) ms i) (add1 i)))))
  
  (define (standard-deviation lst)
    (check-list lst "lst" "(standard-deviation lst)")
    (sqrt (variance lst)))

  (define (rep n lst type)
    ;; returns the appended list formed by repeating the values in lst either n times or n times each
    ;; replicates behavior of rep in R
    (let ([proc-string "(rep n lst type)"])
      (check-positive-integer n "n" proc-string)
      (unless (list? lst)
        (assertion-violation
         proc-string
         "lst is not a list"))
      (unless (symbol? type)
        (assertion-violation
         proc-string
         "type must be symbol: 'each or 'times"))
      (cond [(= n 1) lst]
            [(= (length lst) 1) (make-list n (car lst))]
            [(symbol=? type 'each)
             (apply append (map (lambda (x) (make-list n x)) lst))]
            [(symbol=? type 'times)
             (rep-times n lst)]
            [else
             (assertion-violation
              proc-string
              "type must be one of these symbols: 'each or 'times")])))

  (define (rep-times n lst)
    (let loop ([lst-out lst]
               [n n])
      (if (= n 1) lst-out
          (loop (append lst lst-out) (sub1 n)))))

  (define rank
    (case-lambda
      [(lst) (rank lst 'min)]
      [(lst ties-method)
       (let ([proc-string "(rank lst ties-method)"])
         (check-list lst "lst" proc-string)
         (unless (and (symbol? ties-method)
                      (member ties-method '(min max mean)))
           (assertion-violation
            proc-string
            "ties-method must be one of these symbols: 'min, 'max, or 'mean")))
       (let* ([sorted-lst (sort < lst)]
              [val-count (rle sorted-lst)]
              [max-count (apply max (map cdr val-count))])
         (if (= max-count 1)
             (rank-simple lst sorted-lst)
             (rank-ties lst sorted-lst val-count ties-method)))]))

  (define (rank-simple lst sorted-lst)
    (let* ([ranks (map add1 (iota (length lst)))]
           [val-rank (map cons sorted-lst ranks)])
      (match-ranks lst val-rank)))

  (define (match-ranks lst val-rank)
    ;; val-rank is a sorted list of pairs with car = value and cdr = rank
    ;; finding rank for each value in original unsorted lst
    (map (lambda (x) (cdr (assoc x val-rank))) lst))

  (define (rank-ties lst sorted-lst val-count ties-method)
    ;; val-count is a list of pairs sorted by value
    (let* ([ranks (rank-val-count val-count ties-method)]
           [val-rank (map cons sorted-lst ranks)])
      (match-ranks lst val-rank)))

  (define (rank-val-count val-count ties-method)
    (let loop ([vc val-count]
               [ranks '()])
      (cond [(null? (cdr vc))
             (reverse (rvc-helper (car vc) ranks ties-method))]
            [else
             (loop (cdr vc) (rvc-helper (car vc) ranks ties-method))])))

  (define (rvc-helper val-count-pair ranks ties-method)
    ;; ranks list is being built up in rank-val-count via calls to rvc-helper
    (cond [(= (cdr val-count-pair) 1)           ;; if a value only occurs once no ties need to be handled
           (cons (add1 (length ranks)) ranks)]  ;; the rank of that value is the position in the ranks list (1-indexed)
          [else
           (append
            (handle-ties (get-next-ranks ranks (cdr val-count-pair)) ties-method)
            ranks)]))

  (define (get-next-ranks ranks count)
    ;; count is the cdr of a val-count-pair
    ;; given list of ranks, e.g., '(1 2 3), and a count, e.g., 2,
    ;; then returns '(4 5)
    (map (lambda (x) (+ x (length ranks) 1)) (iota count)))

  (define (handle-ties ranks ties-method)
    ;; in R, rank includes random, first, last as ties methods
    ;; they are a little trickier to implement so not including here
    (let* ([n (length ranks)]
           [proc-lookup
            (list (cons 'mean (lambda (x) (mean x)))
                  (cons 'min (lambda (x) (apply min x)))
                  (cons 'max (lambda (x) (apply max x))))])
      ;; the ranks for ties are repeated (hence make-list)
      ;; e.g., if the sorted list is '(1 2 2) then the naive ranks are '(1 2 3)
      ;; and 'min = '(1 2 2), 'max = '(1 3 3), 'mean = '(1 5/2 5/2)
      (make-list n ((cdr (assoc ties-method proc-lookup)) ranks))))

  (define (correlation x y method)
    (let ([proc-string "(correlation x y method)"])
      (check-list x "x" proc-string)
      (check-list y "y" proc-string)
      (unless (= (length x) (length y))
        (assertion-violation proc-string  "x and y must be same length"))
      (cond [(symbol=? method 'pearson)
             (pearson x y)]
            [(symbol=? method 'spearman)
             (pearson (rank x 'mean) (rank y 'mean))]
            [(symbol=? method 'kendall)
             (exact->inexact (kendall x y))]
            [else
             (assertion-violation
              proc-string
              "method must be one of these symbols: 'pearson, 'spearman, or 'kendall")]))) 
  
  (define (pearson x y)
    (let ([n (length x)]
          [x-sum (apply + x)]
          [y-sum (apply + y)]
          [xy-sum (apply
                   + (map (lambda (x-val y-val)
                            (* x-val y-val))
                          x y))]
          [x2-sum (sum-squares x)]
          [y2-sum (sum-squares y)])
      (/ (- (* n xy-sum) (* x-sum y-sum))
         (* (sqrt (- (* n x2-sum) (expt x-sum 2)))
            (sqrt (- (* n y2-sum) (expt y-sum 2)))))))

  (define (sum-squares lst)
    (apply + (map (lambda (x) (expt x 2)) lst)))

  (define (kendall x y)
    (let* ([pairs (sort-two-lists (rank x 'mean)
                                  (rank y 'mean))]
           [x-ranks (map car pairs)]
           [y-ranks (map cdr pairs)]
           [n (length x-ranks)]
           [N (/ (* n (sub1 n)) 2)])
      (let-values ([(C D x-ties y-ties)
                    (concordance x-ranks y-ranks)])
        (/ (- C D) (sqrt (* (- N x-ties) (- N y-ties)))))))
  
  (define (concordance x y)
    (let loop ([x x]
               [y y]
               [C 0]
               [D 0]
               [x-ties 0]
               [y-ties 0])
      (cond [(null? (cdr x))
             (values C D x-ties y-ties)]
            [else
             (loop (cdr x) (cdr y)
                   (+ C (cordant-count x y 1))
                   (+ D (cordant-count x y -1))
                   (+ x-ties (ties-count x))
                   (+ y-ties (ties-count y)))])))
  
  (define (cordant-count x y sign)
    ;; sign = -1, 1
    (length
     (filter (lambda (a)
               (= a sign))
             (map * (diff-first-sign x) (diff-first-sign y)))))

  (define (ties-count lst)
    (length (filter (lambda (x) (= x 0)) (diff-first-sign lst))))
  
  (define (diff-first-sign lst)
    (map (lambda (x) (sign (- (car lst) x))) (cdr lst)))
  
  (define (sort-two-lists x y)
    ;; sort two lists in ascending order
    ;; sorts first by x and then by y
    ;; returns sorted list of pairs
    (let* ([unsort-pairs (map cons x y)]
           [swo (sum-weighted-order x y)]
           [swo-up (map cons swo unsort-pairs)])
      (map cdr (sort (lambda (a b) (< (car a) (car b))) swo-up))))
  
  (define (sum-weighted-order x y)
    ;; initial weight of 100 is arbitrary
    (let ([x-weight (weighted-order x 100)]
          [y-weight (weighted-order y (/ 100 (length (unique y))))])
      (map + x-weight y-weight)))

  (define (weighted-order lst weight)
    ;; returns list of weighted order values for every value in lst 
    (let* ([unique-sorted (unique lst)]
           [ranks (map (lambda (x) (* x weight)) (enumerate unique-sorted))]
           [lookup (map cons unique-sorted ranks)]) 
      (map (lambda (x) (cdr (assoc x lookup))) lst)))

  (define (diff lst)
    ;; returns list of differences (with lag of one) for all elements in lst
    (check-list lst "lst" "(diff lst)")
    (let loop ([lst lst]
               [out '()])
      (cond [(null? (cdr lst))
             (reverse out)]
            [(loop (cdr lst)
                   (cons (- (cadr lst) (car lst)) out))])))

  (define (sign x)
    (cond [(< x 0) -1]
          [(> x 0) 1]
          [else 0]))
  
  )


