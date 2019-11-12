(library (chez-stats chez-stats)
  (export
   ;; statistics
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
   ;; csv
   preview-csv
   read-csv
   write-csv
   ;; manipulation
   drop-columns
   rename-columns
   select-columns
  )

  (import (chezscheme)
	  (chez-stats statistics)
	  (chez-stats random-variates)
	  (chez-stats csv)
	  (chez-stats manipulation))

  )
