(library (chez-stats random)
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
   random-uniform
   repeat)

  (import (chezscheme)
	  (chez-stats assertions))

  (define (repeat n thunk)
    (let loop ([i 0]
               [result '()])
      (if (= i n)
          result
          (loop (add1 i) (cons (thunk) result)))))

  ;; equivalent result to (apply + (repeat...))
  ;; but presumably more efficient because not building up a list
  ;; and requires that the thunk returns a number
  ;; only used in random-binomial
  (define (repeat-sum n thunk)
    (let loop ([i 0]
               [result 0])
      (if (= i n)
          result
          (loop (add1 i) (+ (thunk) result)))))
  
  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-bernoulli p)
    (check-p p "(random-bernoulli p)")
    (if (<= (random 1.0) p) 1 0))
  
  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pd
  (define (random-binomial trials p)
    (let ([proc-string "(random-binomial trials p)"])
      (check-integer-gte-zero trials "trials" proc-string)
      (check-p p proc-string))
    (repeat-sum trials (lambda () (random-bernoulli p))))

  ;; same algorithm as rmultinom in R
  ;; https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Multinom.html
  (define (random-multinomial trials ps)
    ;; rescale ps (if necessary)
    (define (scale-ps ps)
      (let ([p-sum (apply + ps)])
	(if (= p-sum 1)
	    ps
	    (map (lambda (p) (/ p p-sum)) ps))))
    (let ([proc-string "(random-multinomial trials ps)"])
      (check-integer-gte-zero trials "trials" proc-string)
      (check-list ps "ps" proc-string))
    (let loop ([p-scaled (scale-ps ps)]
               [p-used '()]
               [results '()])
      (if (null? p-scaled)
          (reverse results)
          (let* ([p-now (car p-scaled)]
                 [p-now-adj-temp (/ p-now (- 1 (apply + p-used)))]
                 [p-now-adj (if (> p-now-adj-temp 1) 1 p-now-adj-temp)]
                 [result-next (random-binomial (- trials (apply + results)) p-now-adj)])
            (loop (cdr p-scaled) (cons p-now p-used) (cons result-next results))))))

  ;; SRFI 27
  (define (random-exponential mu)
    (check-positive-real mu "mu" "(random-exponential mu)")
    (- (* mu (log (random 1.0)))))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-geometric p)
    (check-p-exclusive p "(random-geometric p)")
    (ceiling (/ (log (random 1.0)) (log (- 1 p)))))

  (define random-normal
    (case-lambda
      [() (rnorm 0 1)]
      [(mu) (rnorm mu 1)]
      [(mu sd) (rnorm mu sd)]))

  ;; rejection method from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (rnorm mu sd)
    (let ([proc-string "(random-normal mu sd)"])
      (check-real mu "mu" proc-string)
      (check-real-gte-zero sd "sd" proc-string))
    (let loop ()
      (let* ([u1 (random 1.0)]
             [u2 (random 1.0)]
             [x (* -1 (log u1))])
        (if (> u2 (exp (/ (* -1 (expt (sub1 x) 2)) 2)))
            (loop)
            (let ([u3 (random 1.0)])
              (if (> u3 0.5)
                  (+ mu (* sd x))
                  (- mu (* sd x))))))))

  ;; Marsaglia and Tsang’s Method
  ;; from https://www.hongliangjie.com/2012/12/19/how-to-generate-gamma-random-variables/
  ;; material at url refers to rate parameter as scale
  (define (random-gamma shape rate)
    (let ([proc-string "(random-gamma shape rate)"])
      (check-positive-real shape "shape" proc-string)
      (check-positive-real rate "rate" proc-string))
    (let loop ([shape shape]
               [rate rate])
      (if (not (= rate 1))
          (/ (loop shape 1) rate)
          (if (< shape 1)
              (* (loop (add1 shape) rate)
                 (expt (random 1.0) (/ 1 shape)))
              (let* ([d (- shape 1/3)]
                     [c (/ 1 (sqrt (* 9 d)))]
                     [Z (random-normal)]
                     [U (random 1.0)]
                     [V (expt (add1 (* c Z)) 3)]
                     [z-comp (/ -1 c)]
                     [log-U-comp (+ (* 1/2 (expt Z 2))
                                    (- d (* d V))
                                    (* d (log V)))])
                (if (and (> Z z-comp) (< (log U) log-U-comp))
                    (* d V)
                    (loop shape rate)))))))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-beta a b)
    (let ([proc-string "(random-beta a b)"])
      (check-positive-real a "a" proc-string)
      (check-positive-real b "b" proc-string))
    (let ([A (random-gamma a 1)]
	  [B (random-gamma b 1)])
      (/ A (+ A B))))

  (define (random-beta-binomial trials p dispersion)
    (let ([proc-string "(random-beta-binomial trials p dispersion)"])
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
      (random-binomial trials (random-beta a b))))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-lognormal mulog sdlog)
    (let ([proc-string "(random-lognormal n mulog sdlog)"])
      (check-real mulog "mulog" proc-string)
      (check-real-gte-zero sdlog "sdlog" proc-string))
    (exp (+ mulog (* sdlog (random-normal)))))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-pareto shape)
    (check-positive-real shape "shape" "(random-pareto shape)")
    (/ 1 (expt (random 1.0) (/ 1 shape))))

  ;; https://en.wikipedia.org/wiki/Poisson_distribution
  ;; parameter is conventionally called lambda but using mu to avoid confusion with how lambda is used in scheme
  (define (random-poisson mu)
    (define (update-ps ps i)
      (let* ([p (car ps)]
             [s (cdr ps)]
             [p-new (* p (/ mu i))])
        (cons p-new (+ s p-new))))
    (check-real-gte-zero mu "mu" "(random-poisson mu)")
    (let ([u (random 1.0)]
          [p-init (exp (* -1 mu))])
      (let loop ([ps (cons p-init p-init)]
                 [result 1])
        (if (>= (cdr ps) u)
            (sub1 result)
            (loop (update-ps ps result) (add1 result))))))

  ;; from https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
  (define (random-negative-binomial trials p)
    (let ([proc-string "(random-negative-binomial trials p)"])
      (check-p-exclusive p proc-string)
      (check-positive-real trials "trials" proc-string))
    (let ([gamma (random-gamma trials (/ p (- 1 p)))])
      (random-poisson gamma)))

  (define (random-uniform mn mx)
    (let ([proc-string "(random-uniform mn mx)"])
      (check-real mn "mn" proc-string)
      (check-real mx "mx" proc-string)
      (unless (> mx mn)
	(assertion-violation proc-string "mx is not greater than mn")))
    (+ mn (* (- mx mn) (random 1.0))))

  )
