(library (chez-stats chez-stats)
  (export
   ;; csv
   preview-csv
   read-csv
   write-csv
   ;; dataframe
   ->
   and2
   or2
   dataframe?
   dataframe-add
   dataframe-append
   dataframe-alist
   dataframe-contains?
   dataframe-dim
   dataframe-drop
   dataframe-equal?
   dataframe-filter
   dataframe-groups
   dataframe-head
   dataframe-map
   dataframe-names
   dataframe-partition
   dataframe-rename
   dataframe-read
   dataframe-select
   dataframe-update
   dataframe-values
   dataframe-write
   make-dataframe
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
   weighted-mean)

  (import (chezscheme)
	  (chez-stats csv)
	  (chez-stats dataframe)
	  (chez-stats random-variates)
	  (chez-stats statistics))

  )
