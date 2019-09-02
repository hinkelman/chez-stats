(library (chez-stats random-variates)
  (export
   random-bernoulli
   random-binomial
   random-exponential
   random-poisson
   random-normal)

  (import (chezscheme)
	  (chez-stats assertions))

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

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-binomial n trials p)
    (define (rbin trials p)
      (apply + (random-bernoulli trials p)))
    (let ([proc-string "(random-binomial n trials p)"])
      (check-positive-integer n "n" proc-string)
      (check-positive-integer trials "trials" proc-string)
      (check-p p proc-string))
    (build-random-list n (lambda () (rbin trials p))))

  ;; SRFI 27
  (define (random-exponential n mu)
    (define (rexp mu)
      (- (* mu (log (random 1.0)))))
    (let ([proc-string "(random-exponential n mu)"])
      (check-positive-integer n "n" proc-string)
      (check-positive-real mu "mu" proc-string))
    (build-random-list n (lambda () (rexp mu))))

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
  
  )
