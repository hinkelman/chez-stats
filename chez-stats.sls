(library (chez-stats)
  (export
   ;; csv
   preview-csv
   preview-tsv
   read-csv
   read-tsv
   write-csv
   ;; random-variates
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
