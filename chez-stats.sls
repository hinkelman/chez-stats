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
   random-sample
   random-uniform
   repeat
   ;; statistics
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
   shuffle
   sign
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
