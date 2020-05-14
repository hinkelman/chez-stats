(library (chez-stats)
  (export
   ;; delimited
   read-delim
   write-delim
   ;; random
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
   repeat
   ;; statistics
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
	  (chez-stats delimited)
	  (chez-stats random)
	  (chez-stats statistics))
  )
