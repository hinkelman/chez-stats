(library (chez-stats chez-stats)
  (export
   ;; csv
   preview-csv
   preview-tsv
   read-csv
   read-tsv
   write-csv
   ;; dataframe
   ->
   ->>
   $
   rowtable->dataframe
   dataframe->rowtable
   dataframe?
   dataframe-append
   dataframe-append-all
   dataframe-alist
   dataframe-contains?
   dataframe-dim
   dataframe-drop
   dataframe-equal?
   dataframe-filter
   dataframe-head
   dataframe-list-modify
   dataframe-modify
   dataframe-names
   dataframe-names-update
   dataframe-partition
   dataframe-rename
   dataframe-read
   dataframe-select
   dataframe-split
   dataframe-tail
   dataframe-unique
   dataframe-values
   dataframe-write
   filter-expr
   make-dataframe
   modify-expr
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
