(library (statistics statistics)
  (export
   count
   cumulative-sum
   ecdf
   interquartile-range
   mean
   median
   mode
   quantile
   range
   standard-deviation
   unique
   variance
   weighted-mean)

  (import (chezscheme))
  
  (define (list-check ls who)
    (unless (for-all real? ls) (assertion-violation who "all elements of list must be real numbers;" ls))
    (when (null? ls) (assertion-violation who "list is empty;" ls)))

  (define (count ls)
    (list-check ls "(count)")
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
    (list-check ls "(mode)")
    (let* ([val-count (count ls)]
	   [mx (apply max (cadr val-count))])
      (filter (lambda (x) (not (null? x)))
	      (map (lambda (val count) (if (= count mx) val '()))
		   (car val-count)
		   (cadr val-count)))))

  (define (unique ls)
    (list-check ls "(unique)")
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
    (define gamma-proc
      (cond
       [(= type 1) (lambda (g j) (if (= g 0) 0 1))]
       [(= type 2) (lambda (g j) (if (= g 0) 0.5 1))]
       [(= type 3) (lambda (g j) (if (and (= g 0) (even? j)) 0 1))]
       [else (lambda (g j) g)]))
    (list-check ls "(quantile)")
    (when (or (< type 1) (> type 9) (not (integer? type))) (assertion-violation "(quantile)" "type must be integer from 1-9;" type))
    (when (or (< p 0) (> p 1)) (assertion-violation "(quantile)" "p is outside [0,1];" p))
    (let* ([n (length ls)]
	   [order-stats (unique ls)]
	   [ms (list 0 0 -1/2 0 1/2 p (- 1 p) (* (add1 p) 1/3) (+ (* p 1/4) 3/8))]
	   [m (list-ref ms (sub1 type))]
	   [j (floor (+ (* n p) m))]
	   [g (- (+ (* n p) m) j)]
	   [gamma (gamma-proc g j)])
      (calc-Q order-stats (inexact->exact j) gamma)))

					;(map (lambda (x) (quantile '(1 2 3 4 5 6) x 9)) '(#e0.0 #e0.1 #e0.2 #e0.3 #e0.4 #e0.5 #e0.6 #e0.7 #e0.8 #e0.9 #e1.0))

  (define (median ls)
    (list-check ls "(median)")
    (quantile ls 0.5 7))

  (define (interquartile-range ls type)
    (list-check ls "(interquartile-range)")
    (- (quantile ls 0.75 type) (quantile ls 0.25 type)))

  (define (cumulative-sum ls)
    (define (iterate ls result total)
      (cond
       [(null? ls) (reverse result)]
       [else
	(unless (for-all real? ls) (assertion-violation "(cumulative-sum)" "all elements of list must be real numbers;" ls))
	(let ([new-total (+ (car ls) total)])
	  (iterate (cdr ls) (cons new-total result) new-total))]))
    (iterate ls '() 0))

  (define (ecdf ls)
    (list-check ls "(ecdf)")
    (let* ([n (length ls)]
	   [val-count (count ls)]
	   [cs (cumulative-sum (cadr val-count))])
      (list (car val-count)
	    (map (lambda (x) (/ x n)) cs))))

  (define (range ls)
    (list-check ls "(range)")
    (cons (apply min ls) (apply max ls)))

  (define (mean ls)
    (list-check ls "(mean)")
    (/ (apply + ls) (length ls)))

  (define (weighted-mean ls weights)
    ;; these list checks should be tailored more to each list
    (list-check ls "(weighted-mean)")
    (list-check weights "(weighted-mean)")
    (/ (apply + (map (lambda (x y) (* x y)) ls weights)) (apply + weights)))
  
  ;; Welford's algorithm
  ;; https://www.johndcook.com/blog/standard_deviation/
  ;; variance is much slower than mean
  (define (variance ls)
    (define (update-ms lsi ms i)
      (let* ([m (car ms)]
	     [s (cdr ms)]
	     [new-m (+ m (/ (- lsi m) i))])
	(cons new-m
	      (+ s (* (- lsi m) (- lsi new-m))))))		     
    (define (iterate ls ms i)
      (list-check ls "(variance)")
      (cond
       ;; when exit statement is reached, i is (length x) + 1
       ;; need to subtract 2 to get (length x) - 1 (i.e., sample variation)
       [(null? ls)  (/ (cdr ms) (- i 2.0))]
       [else (iterate (cdr ls) (update-ms (car ls) ms i) (add1 i))]))
    (iterate (cdr ls) (cons (car ls) 0) 2))

  (define (standard-deviation ls)
    (list-check ls "(standard-deviation)")
    (sqrt (variance ls)))
  )


