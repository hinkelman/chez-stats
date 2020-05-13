(library (chez-stats random-variates)
  (export
   random-bernoulli
   random-beta
   random-beta-binomial
   random-binomial
   random-exponential
   random-gamma
   random-geometric
   random-lognormal
   random-multinomial
   random-negative-binomial
   random-normal
   random-pareto
   random-poisson
   random-uniform)

  (import (chezscheme)
	  (chez-stats assertions))

  ;; in some simple deleted time tests, this recursive version of build-random-list
  ;; was about 3x faster than a version that used fold-right and (make-list)
  ;; when drawing 10 million random-bernoulli values
  (define (build-random-list n proc)
    (define (iterate result i)
      (if (= i n)
	  result
	  (iterate (cons (proc) result) (add1 i))))
    (iterate '() 0))
 
  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-bernoulli n p)
    (define (rbern p)
      (if (<= (random 1.0) p) 1 0))
    (let ([proc-string "(random-bernoulli n p)"])
      (check-positive-integer n "n" proc-string)
      (check-p p proc-string))
    (build-random-list n (lambda () (rbern p))))
  
  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pd
  (define (random-binomial n trials p)
    (define (rbinom trials p)
      (apply + (random-bernoulli trials p)))
    (let ([proc-string "(random-binomial n trials p)"])
      (check-positive-integer n "n" proc-string)
      (check-positive-integer trials "trials" proc-string)
      (check-p p proc-string))
    (build-random-list n (lambda () (rbinom trials p))))

  ;; same algorithm as rmultinom in R
  ;; https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Multinom.html
  (define (random-multinomial trials ps)
    ;; rescale ps (if necessary)
    (define (scale-ps ps)
      (let ([p-sum (apply + ps)])
	(if (= p-sum 1)
	    ps
	    (map (lambda (p) (/ p p-sum)) ps))))
    (define (iterate p-scaled p-used results)
      (cond
       [(null? p-scaled)
	(reverse results)]
       [else
	(let* ([p-now (car p-scaled)]
	       [p-now-adj-temp (/ p-now (- 1 (apply + p-used)))]
	       ;; floating point arithmetic was producing values of p-now-adj > 1
	       ;; added condition for 0 as a precaution; not sure if it is necessary
	       [p-now-adj (cond [(< p-now-adj-temp 0) 0]
				[(> p-now-adj-temp 1) 1]
				[else p-now-adj-temp])]
	       [result-next (random-binomial 1 (- trials (apply + results)) p-now-adj)])
	  (iterate (cdr p-scaled) (cons p-now p-used) (cons (car result-next) results)))]))
    (let ([proc-string "(random-multinomial trials ps)"])
      (check-positive-integer trials "trials" proc-string)
      (check-list ps "ps" proc-string))
    (iterate (scale-ps ps) '() '()))

  ;; SRFI 27
  (define (random-exponential n mu)
    (define (rexp mu)
      (- (* mu (log (random 1.0)))))
    (let ([proc-string "(random-exponential n mu)"])
      (check-positive-integer n "n" proc-string)
      (check-positive-real mu "mu" proc-string))
    (build-random-list n (lambda () (rexp mu))))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-geometric n p)
    (define (rgeom p)
      (ceiling (/ (log (random 1.0)) (log (- 1 p)))))
    (let ([proc-string "(random-geometric n p)"])
      (check-positive-integer n "n" proc-string)
      (check-p-exclusive p proc-string))
    (build-random-list n (lambda () (rgeom p))))

  ;; rejection method from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-normal n mu sd)
    (define (rnorm mu sd)
      (let iterate ()
	(let* ([u1 (random 1.0)]
	       [u2 (random 1.0)]
	       [x (* -1 (log u1))])
	  (if (> u2 (exp (/ (* -1 (expt (sub1 x) 2)) 2)))
	      (iterate)
	      (let ([u3 (random 1.0)])
		(if (> u3 0.5)
		    (+ mu (* sd x))
		    (- mu (* sd x))))))))
    (let ([proc-string "(random-normal n mu sd)"])
      (check-positive-integer n "n" proc-string)
      (check-real mu "mu" proc-string)
      (check-real-gte-zero sd "sd" proc-string))
    (build-random-list n (lambda () (rnorm mu sd))))

  ;; Marsaglia and Tsang’s Method
  ;; from https://www.hongliangjie.com/2012/12/19/how-to-generate-gamma-random-variables/
  ;; material at url refers to rate parameter as scale
  (define (random-gamma n shape rate)
    (define (rgamma shape rate)
      (if (not (= rate 1))
          (/ (rgamma shape 1) rate)
          (if (< shape 1)
              (* (rgamma (add1 shape) rate)
                 (expt (random 1.0) (/ 1 shape)))
              (let* ([d (- shape 1/3)]
                     [c (/ 1 (sqrt (* 9 d)))]
                     [Z (list-ref (random-normal 1 0 1) 0)]
                     [U (random 1.0)]
                     [V (expt (add1 (* c Z)) 3)]
                     [z-comp (/ -1 c)]
                     [log-U-comp (+ (* 1/2 (expt Z 2))
                                    (- d (* d V))
                                    (* d (log V)))])
                (if (and (> Z z-comp) (< (log U) log-U-comp))
                    (* d V)
                    (rgamma shape rate))))))
    (let ([proc-string "(random-gamma n shape rate)"])
      (check-positive-integer n "n" proc-string)
      (check-positive-real shape "shape" proc-string)
      (check-positive-real rate "rate" proc-string))
    (build-random-list n (lambda () (rgamma shape rate))))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-beta n a b)
    (let ([proc-string "(random-beta n a b)"])
      (check-positive-integer n "n" proc-string)
      (check-positive-real a "a" proc-string)
      (check-positive-real b "b" proc-string))
    (let ([A (random-gamma n a 1)]
	  [B (random-gamma n b 1)])
      (map (lambda (x y) (/ x (+ x y))) A B)))

  (define (random-beta-binomial n trials p dispersion)
    (let ([proc-string "(random-beta-binomial n trials p dispersion)"])
      (check-positive-integer n "n" proc-string)
      (check-positive-integer trials "trials" proc-string)
      (check-p-exclusive p proc-string)
      (unless (> dispersion 1)
	(assertion-violation proc-string "dispersion is not a real number > 1"))
      ;; as difference between trials and dispersion approaches zero
      ;; will get an error from (random-binomial) about invalid values of p
      (unless (> trials dispersion)
    	(assertion-violation proc-string "trials is not greater than dispersion")))
    (let* ([g (/ (- trials dispersion) (sub1 dispersion))]
	   [a (* g p)]
	   [b (* g (- 1 p))])
      (map (lambda (x) (list-ref (random-binomial 1 trials x) 0))
  	   (random-beta n a b))))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-lognormal n mulog sdlog)
    (let ([proc-string "(random-lognormal n mulog sdlog)"])
      (check-positive-integer n "n" proc-string)
      (check-real mulog "mulog" proc-string)
      (check-real-gte-zero sdlog "sdlog" proc-string))
    (map (lambda (x) (exp (+ mulog (* sdlog x)))) (random-normal n 0 1)))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-pareto n shape)
    (define (rpareto shape)
      (/ 1 (expt (random 1.0) (/ 1 shape))))
    (let ([proc-string "(random-pareto n shape)"])
      (check-positive-integer n "n" proc-string)
      (check-positive-real shape "shape" proc-string))
    (build-random-list n (lambda () (rpareto shape))))

  ;; https://en.wikipedia.org/wiki/Poisson_distribution
  ;; parameter is conventionally called lambda but using mu to avoid confusion with how lambda is used in scheme
  (define (random-poisson n mu)
    (define (rpois mu)
      (define u (random 1.0))
      (define p-init (exp (* -1 mu)))
      (define (update-ps ps i)
	(let* ([p (car ps)]
	       [s (cdr ps)]
	       [p-new (* p (/ mu i))])
	  (cons p-new (+ s p-new))))
      (define (iterate ps i)
	(cond [(>= (cdr ps) u) (sub1 i)]
	      [else (iterate (update-ps ps i) (add1 i))]))
      (iterate (cons p-init p-init) 1))
    (let ([proc-string "(random-poisson n mu)"])
      (check-positive-integer n "n" proc-string)
      (check-real-gte-zero mu "mu" proc-string))
    (build-random-list n (lambda () (rpois mu))))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-negative-binomial n trials p)
    (let ([proc-string "(random-negative-binomial n trials p)"])
      (check-positive-integer n "n" proc-string)
      (check-p-exclusive p proc-string)
      (check-positive-real trials "trials" proc-string))
    (let ([gamma-list (random-gamma n trials (/ p (- 1 p)))])
      (map (lambda (x) (list-ref (random-poisson 1 x) 0)) gamma-list)))

  (define (random-uniform n mn mx)
    (define (runif mn mx)
      (+ mn (* (- mx mn) (random 1.0))))
    (let ([proc-string "(random-uniform n mn mx)"])
      (check-positive-integer n "n" proc-string)
      (check-real mn "mn" proc-string)
      (check-real mx "mx" proc-string)
      (unless (> mx mn)
	(assertion-violation proc-string "mx is not greater than mn")))
    (build-random-list n (lambda () (runif mn mx))))

  )
