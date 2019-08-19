;; Slides describing algorithms found at https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf

(load "../descriptive-statistics/descriptive-statistics.ss")

(define (build-random-list n proc)
  (define (iterate result i)
    (if (= i n)
        result
        (iterate (cons (proc) result) (add1 i))))
  (iterate '() 0))

;; from Jain slides
;; should check that 0 >= p <= 1
(define (random-bernoulli n p)
  (define (rbern p)
    (if (<= (random 1.0) p) 1 0))
  (build-random-list n (lambda () (rbern p))))

(define bernoulli-list (random-bernoulli 1e5 0.2))
(real->flonum (mean bernoulli-list))  ;; should be 0.2 when p = 0.2
(variance bernoulli-list)             ;; should be 0.16 when p = 0.2

;; from Jain slides
(define (random-binomial n trials p)
  (define (rbin trials p)
    (apply + (random-bernoulli trials p)))
  (build-random-list n (lambda () (rbin trials p))))

(define binomial-list (random-binomial 1e5 10 0.5))
(real->flonum (mean binomial-list))   ;; should be 5 when trials = 10 and p = 0.5
(variance binomial-list)              ;; should be 2.5 when trials = 10 and p = 0.5

;; modified from SRFI 27
(define (random-exponential n mu)
  (define (rexp mu)
    (- (* mu (log (random 1.0)))))
  (build-random-list n (lambda () (rexp mu))))

(define exponential-list (random-exponential 1e5 10))
(mean exponential-list)                  ;; should be 10 when mu = 10
(variance exponential-list)              ;; should be 100 when mu = 10

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
  (build-random-list n (lambda () (rpois mu))))

(define poisson-list (random-poisson 1e5 7))
(real->flonum (mean poisson-list))
(variance poisson-list)
