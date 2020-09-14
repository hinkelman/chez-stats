(library (chez-stats statistics)
  (export
   count-unique
   cumulative-sum
   kurtosis
   mean
   median
   mode
   quantile
   rank
   rep
   rle
   standard-deviation
   skewness
   unique
   variance
   weighted-mean)

  (import (chezscheme)
	  (chez-stats assertions))

  (define (rle ls)
    (check-list ls "ls" "(rle ls)")
    (let loop ([first (car ls)]
               [rest (cdr ls)]
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
  
  (define (count-unique ls)
    (check-list ls "ls" "(count-unique ls)")
    (rle (sort < ls)))
  
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

  (define (rep n ls type)
    (let ([proc-string "(rep n ls type)"])
      (check-positive-integer n "n" proc-string)
      (unless (list? ls)
        (assertion-violation
         proc-string
         "ls is not a list"))
      (unless (symbol? type)
        (assertion-violation
         proc-string
         "type must be symbol: 'each or 'times"))
      (cond [(= n 1) ls]
            [(= (length ls) 1) (make-list n (car ls))]
            [(symbol=? type 'each)
             (apply append (map (lambda (x) (make-list n x)) ls))]
            [(symbol=? type 'times)
             (rep-times n ls)]
            [else
             (assertion-violation
              proc-string
              "type must be symbol: 'each or 'times")])))

  (define (rep-times n ls)
    (let loop ([ls-out ls]
               [n n])
      (if (= n 1) ls-out
          (loop (append ls ls-out) (sub1 n)))))

  (define rank
    (case-lambda
      [(ls) (rank ls 'min)]
      [(ls ties-method)
       (let ([proc-string "(rank ls ties-method)"])
         (check-list ls "ls" proc-string)
         (unless (and (symbol? ties-method)
                      (member ties-method '(min max mean)))
           (assertion-violation
            proc-string
            "ties-method must be symbol: 'min, 'max', or 'mean")))
       (let* ([sorted-ls (sort < ls)]
              [val-count (rle sorted-ls)]
              [max-count (apply max (map cdr val-count))])
         (if (= max-count 1)
             (rank-simple ls sorted-ls)
             (rank-ties ls sorted-ls val-count ties-method)))]))

  (define (rank-simple ls sorted-ls)
    (let* ([ranks (map add1 (iota (length ls)))]
           [val-rank (map cons sorted-ls ranks)])
      (match-ranks ls val-rank)))

  (define (match-ranks ls val-rank)
    (map (lambda (x) (cdr (assoc x val-rank))) ls))

  ;; val-count is a list of pairs
  (define (rank-ties ls sorted-ls val-count ties-method)
    (define (iterate val-count ranks)
      (cond [(null? (cdr val-count))
             (reverse (rank-ties-helper (car val-count) ranks ties-method))]
            [else
             (iterate (cdr val-count)
                      (rank-ties-helper (car val-count) ranks ties-method))]))
    (let* ([ranks (iterate val-count '())]
           [val-rank (map cons sorted-ls ranks)])
      (match-ranks ls val-rank)))

  (define (rank-ties-helper val-count-pair ranks ties-method)
    (cond [(= (cdr val-count-pair) 1)
           (cons (add1 (length ranks)) ranks)]
          [else
           (append
            (handle-ties (get-ranks ranks (cdr val-count-pair)) ties-method)
            ranks)]))

  (define (get-ranks ranks len)
    (map (lambda (x)
           (+ x (length ranks) 1))
         (iota len)))

  ;; in R, rank includes random, first, last as ties methods
  ;; they are a little trickier to implement so not including here
  (define (handle-ties ranks ties-method)
    (let* ([n (length ranks)]
           [proc-lookup
            (list (cons 'mean (lambda (x) (make-list n (mean x))))
                  (cons 'min (lambda (x) (make-list n (apply min x))))
                  (cons 'max (lambda (x) (make-list n (apply max x)))))])
      ((cdr (assoc ties-method proc-lookup)) ranks)))
  
  )


