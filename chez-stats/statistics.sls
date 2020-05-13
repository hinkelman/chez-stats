(library (chez-stats statistics)
  (export
   count-unique
   cumulative-sum
   kurtosis
   mean
   median
   mode
   quantile
   standard-deviation
   skewness
   unique
   variance
   weighted-mean)

  (import (chezscheme)
	  (chez-stats assertions))
  
  (define (count-unique ls)
    (check-list ls "ls" "(count-unique ls)")
    (let ([sorted-list (sort < ls)])
      (define (iterate first rest n vals counts)
	(cond
	 [(null? rest)
	  (map cons
	       (reverse (cons first vals))
	       (reverse (cons n counts)))]
         [(= first (car rest))
	  (iterate (car rest) (cdr rest) (add1 n) vals counts)]
         [else
	  (iterate (car rest) (cdr rest) 1 (cons first vals) (cons n counts))]))
      (iterate (car sorted-list) (cdr sorted-list) 1 '() '())))
  
  (define (mode ls)
    (check-list ls "ls" "(mode ls)")
    (let* ([val-count (count-unique ls)]
	   [mx (apply max (map cdr val-count))])
      (filter (lambda (x) (not (null? x)))
	      (map (lambda (val count) (if (= count mx) val '()))
		   (map car val-count)
		   (map cdr val-count)))))

  (define (unique ls)
    (check-list ls "ls" "(unique ls)")
    (map car (count-unique ls)))

  (define quantile
    (case-lambda
      [(ls p) (quantile-helper ls p 8)]
      [(ls p type) (quantile-helper ls p type)]))

  (define (quantile-helper ls p type)
    (let ([proc-string "(quantile ls p type)"])
      (check-list ls "ls" proc-string)
      (check-p p proc-string)
      (check-quantile-type type proc-string))
    (let* ([n (length ls)]
	   [order-stats (unique ls)]
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
    ;; j is calculated for one-based indexing; need to adjust to zero-based
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

  (define (median ls)
    (check-list ls "ls" "(median ls)")
    (quantile ls 0.5 7))

  (define (cumulative-sum ls)
    (check-list ls "ls" "(cumulative-sum ls)")
    (let loop ([ls ls]
	       [result '()]
	       [total 0])
      (if (null? ls)
	  (reverse result)
	  (let ([new-total (+ (car ls) total)])
	    (loop (cdr ls) (cons new-total result) new-total)))))
	  
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
    ;; ms is a pair of m and s variables
    ;; x is current value of ls in loop
    (define (update-ms x ms i) 
      (let* ([m (car ms)]
	     [s (cdr ms)]
	     [new-m (+ m (/ (- x m) (add1 i)))])
	(cons new-m
	      (+ s (* (- x m) (- x new-m))))))
    (check-list ls "ls" "(variance ls)")
    (let loop ([ls (cdr ls)]
	       [ms (cons (car ls) 0)] 
	       [i 1])                 ; one-based indexing in the algorithm
      (if (null? ls)
	  (/ (cdr ms) (- i 1))
          (loop (cdr ls) (update-ms (car ls) ms i) (add1 i)))))
  
  (define (standard-deviation ls)
    (check-list ls "ls" "(standard-deviation ls)")
    (sqrt (variance ls)))
  )


