(library (chez-stats chez-stats)
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
   weighted-mean
   random-bernoulli
   random-beta
   random-beta-binomial
   random-binomial
   random-exponential
   random-gamma
   random-geometric
   random-lognormal
   random-negative-binomial
   random-normal
   random-pareto
   random-poisson
   random-uniform
   preview-csv
   read-csv
   write-csv)

  (import (chezscheme)
	  (chez-stats statistics)
	  (chez-stats random-variates)
	  (chez-stats csv))

  )
