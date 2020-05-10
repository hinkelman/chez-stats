(library (chez-stats statistics)
  (export
   count
   cumulative-sum
   ecdf
   interquartile-range
   kurtosis
   mean
   median
   mode
   quantile
   range
   standard-deviation
   skewness
   unique
   variance
   weighted-mean)

  (import (chezscheme)
	  (chez-stats assertions))
  
  (define (count ls)
    (check-list ls "ls" "(count ls)")
    (let ([sorted-list (sort < ls)])
      (define (iterate first rest n vals counts)
	(cond
	 [(null? rest)
	  (list (reverse (cons first vals))
	        (reverse (cons n counts)))]
         [(= first (car rest))
	  (iterate (car rest) (cdr rest) (add1 n) vals counts)]
         [else
	  (iterate (car rest) (cdr rest) 1 (cons first vals) (cons n counts))]))
      (iterate (car sorted-list) (cdr sorted-list) 1 '() '())))
  
  (define (mode ls)
    (check-list ls "ls" "(mode ls)")
    (let* ([val-count (count ls)]
	   [mx (apply max (cadr val-count))])
      (filter (lambda (x) (not (null? x)))
	      (map (lambda (val count) (if (= count mx) val '()))
		   (car val-count)
		   (cadr val-count)))))

  (define (unique ls)
    (check-list ls "ls" "(unique ls)")
    (car (count ls)))

  (define (quantile ls p type)
    (define (calc-Q order-stats j gamma)
      ;; j is calculated for one-based indexing; need to adjust to zero-based
      (define n-os (length order-stats))
      (cond
       [(< j 1) (list-ref order-stats 0)]
       [(>= j n-os) (list-ref order-stats (sub1 n-os))]
       [else (+ (* (- 1 gamma) (list-ref order-stats (sub1 j)))
		(* gamma (list-ref order-stats j)))]))
    (define (gamma-proc g j)
      (cond
       [(= type 1) (if (= g 0) 0 1)]
       [(= type 2) (if (= g 0) 0.5 1)]
       [(= type 3) (if (and (= g 0) (even? j)) 0 1)]
       [else g]))
    (let ([proc-string "(quantile ls p type)"])
      (check-list ls "ls" proc-string)
      (check-p p proc-string)
      (check-quantile-type type proc-string))
    (let* ([n (length ls)]
	   [order-stats (unique ls)]
	   ;; ms is list of m values for each quantile type
	   [ms (list 0 0 -1/2 0 1/2 p (- 1 p) (* (add1 p) 1/3) (+ (* p 1/4) 3/8))]
	   [m (list-ref ms (sub1 type))]
	   [j (floor (+ (* n p) m))]
	   [g (- (+ (* n p) m) j)]
	   [gamma (gamma-proc g j)])
      ;; j needs to be exact for indexing
      (calc-Q order-stats (inexact->exact j) gamma)))

  (define (median ls)
    (check-list ls "ls" "(median ls)")
    (quantile ls 0.5 7))

  (define (interquartile-range ls type)
    (let ([proc-string "(interquartile-range ls type)"])
      (check-list ls "ls" proc-string)
      (check-quantile-type type proc-string))
    (- (quantile ls 0.75 type) (quantile ls 0.25 type)))

  (define (cumulative-sum ls)
    (define (iterate ls result total)
      (cond
       [(null? ls) (reverse result)]
       [else
	(let ([new-total (+ (car ls) total)])
	  (iterate (cdr ls) (cons new-total result) new-total))]))
    (check-list ls "ls" "(cumulative-sum ls)")
    (iterate ls '() 0))

  (define (ecdf ls)
    (check-list ls "ls" "(ecdf ls)")
    (let* ([n (length ls)]
	   [val-count (count ls)]
	   [cs (cumulative-sum (cadr val-count))])
      (list (car val-count)
	    (map (lambda (x) (/ x n)) cs))))

  (define (range ls)
    (check-list ls "ls" "(range ls)")
    (cons (apply min ls) (apply max ls)))

  (define (mean ls)
    (check-list ls "ls" "(mean ls)")
    (/ (apply + ls) (length ls)))

  (define (skewness ls)
    (check-list ls "ls" "(skewness ls)")
    (let* ([n (length ls)]
	   [x-bar (mean ls)]
	   [x-diff (map (lambda (x) (- x x-bar)) ls)]
	   [num (/ (apply + (map (lambda (x) (expt x 3)) x-diff)) n)]
	   [den (/ (apply + (map (lambda (x) (expt x 2)) x-diff)) n)])
      (/ num (expt den (/ 3 2)))))

  (define (kurtosis ls)
    (check-list ls "ls" "(kurtosis ls)")
    (let* ([n (length ls)]
	   [x-bar (mean ls)]
	   [x-diff (map (lambda (x) (- x x-bar)) ls)]
	   [num (apply + (map (lambda (x) (expt x 4)) x-diff))]
	   [den (expt (apply + (map (lambda (x) (expt x 2)) x-diff)) 2)])
      (* n (/ num den))))

  (define (weighted-mean ls weights)
    (let ([proc-string "(weighted-mean ls weights)"])
      (check-list ls "ls" proc-string )
      (check-list weights "weights" proc-string)
      (unless (= (length ls) (length weights))
	(assertion-violation proc-string "ls and weights are not the same length")))
    (/ (apply + (map (lambda (x y) (* x y)) ls weights)) (apply + weights)))

  (define (variance ls)
    (define (update-ms lsi ms i)
      (let* ([m (car ms)]
	     [s (cdr ms)]
	     [new-m (+ m (/ (- lsi m) (add1 i)))])
	(cons new-m
	      (+ s (* (- lsi m) (- lsi new-m))))))		     
    (define (iterate ls ms i)
      (cond
       [(null? ls)  (/ (cdr ms) (- i 1))]
       [else (iterate (cdr ls) (update-ms (car ls) ms i) (add1 i))]))
    (check-list ls "ls" "(variance ls)")
    (iterate (cdr ls) (cons (car ls) 0) 1))
  
  (define (standard-deviation ls)
    (check-list ls "ls" "(standard-deviation ls)")
    (sqrt (variance ls)))
  )


