# Chez Scheme Statistics Library

Read/write CSV files, compute descriptive statistics, and generate random variates in Chez Scheme.

Related blog posts:  
[Writing a Chez Scheme library](https://www.travishinkelman.com/post/writing-chez-scheme-library/)  
[Reading and writing CSV files in Chez Scheme](https://www.travishinkelman.com/post/reading-writing-csv-files-chez-scheme/)  
[chez-stats is now available through Raven](https://www.travishinkelman.com/post/chez-stats-now-available-through-raven/)

## Installation and Import

### [Raven](http://ravensc.com) Package Manager

```
raven install chez-stats
```

### Manual Installation

```
$ cd ~/scheme # where '~/scheme' is the path to your Chez Scheme libraries
$ git clone git://github.com/hinkelman/chez-stats.git
```

For more information on installing Chez Scheme libraries, see this [blog post](https://www.travishinkelman.com/post/getting-started-with-chez-scheme-and-emacs/).

### Import 

Import all `chez-stats` procedures: `(import (chez-stats chez-stats))`

## Table of Contents

### Descriptive Statistics  

[`(count ls)`](#procedure-count-ls)  
[`(cumulative-sum ls)`](#procedure-cumulative-sum-ls)  
[`(ecdf ls)`](#procedure-ecdf-ls)  
[`(interquartile-range ls type)`](#procedure-interquartile-range-ls-type)  
[`(kurtosis ls)`](#procedure-kurtosis-ls)  
[`(mean ls)`](#procdure-mean-ls)  
[`(median ls)`](#procedure-median-ls)  
[`(mode ls)`](#procedure-mode-ls)  
[`(quantile ls p type)`](#procedure-quantile-ls-p-type)  
[`(range ls)`](#procedure-range-ls)  
[`(skewness ls)`](#procedure-skewness-ls)  
[`(standard-deviation ls)`](#procedure-standard-deviation-ls)  
[`(unique ls)`](#procedure-unique-ls)  
[`(variance ls)`](#procedure-variance-ls)  
[`(weighted-mean ls weights)`](#procedure-weighted-mean-ls-weights)

### Read and Write CSV Files

[`(preview-csv path rows)`](#procedure-preview-csv-path-rows)  
[`(read-csv path)`](#procedure-read-csv-path)  
[`(write-csv ls path overwrite)`](#procedure-write-csv-ls-path-overwrite)  

### Generate Random Variates

[`(random-bernoulli n p)`](#procedure-random-bernoulli-n-p)  
[`(random-beta n a b)`](#procedure-random-beta-n-a-b)  
[`(random-beta-binomial n trials p dispersion)`](#procedure-random-beta-binomial-n-trials-p-dispersion)  
[`(random-binomial n trials p)`](#procedure-random-binomial-n-trials-p)  
[`(random-exponential n mu)`](#procedure-random-exponential-n-mu)  
[`(random-gamma n shape rate)`](#procedure-random-gamma-n-shape-rate)  
[`(random-geometric n p)`](#procedure-random-geometric-n-p)  
[`(random-lognormal n mulog sdlog)`](#procedure-random-lognormal-n-mulog-sdlog)  
[`(random-negative-binomial n trials p)`](#procedure-random-negative-binomial-n-trials-p)  
[`(random-normal n mu sd)`](#procedure-random-normal-n-mu-sd)  
[`(random-pareto n shape)`](#procedure-random-pareto-n-shape)  
[`(random-poisson n mu)`](#procedure-random-poisson-n-mu)  
[`(random-uniform n mn mx)`](#procedure-random-uniform-n-mn-mx)

## Descriptive Statistics

Import only the descriptive statistics procedures: `(import (chez-stats statistics))`

#### procedure: `(count ls)`
**returns:** a list containing a sorted list of the unique values in `ls` and a list of counts corresponding to the unique values

```
> (count '(1 2 3 4 2 1))
((1 2 3 4) (2 2 1 1))
> (count '(1.1 1 2.2 2 1.1 1.1))
((1 1.1 2 2.2) (1 3 1 1))
> (count '(0.5 1/2 #e0.5 1 1 2))
((1/2 1 2) (3 2 1))
> (count '("a" "b" "b" "a"))
Exception in (count ls): at least one element of ls is not a real number
```

#### procedure: `(cumulative-sum ls)`
**returns:** a list that is the cumulative sum of the values in `ls`

```
> (cumulative-sum '(1 2 3 4 5))
(1 3 6 10 15)
> (cumulative-sum '(5 4 3 2 1))
(5 9 12 14 15)
```

#### procedure: `(ecdf ls)`
**returns:** a list containing a sorted list of the unique values in `ls` and list of the empirical cumulative distribution corresponding to the unique values

```
> (ecdf '(1 5 5 5 10))
((1 5 10) (1/5 4/5 1))
> (ecdf '(0.5 2 2 2.5 4 5))
((0.5 2 2.5 4 5) (1/6 1/2 2/3 5/6 1))
> (ecdf '(1/2 0.5 10 10 10 20))
((0.5 10 20) (1/3 5/6 1))
```

#### procedure: `(interquartile-range ls type)`
**returns:** the interquartile range of the values in `ls` for the given `type` ([see quantile](#procedure-quantile-ls-p-type))

```
> (interquartile-range '(1 2 3 4 5 6) 7)
2.5
> (quantile '(1 2 3 4 5 6) 0.75 7)
4.75
> (quantile '(1 2 3 4 5 6) 0.25 7)
2.25
```

#### procedure: `(kurtosis ls)`
**returns:** the kurtosis of the values in `ls`

```
> (kurtosis '(-10 0 10))
3/2
> (kurtosis '(1 2 2 3 3 3))
51/25
```

#### procedure: `(mean ls)`
**returns:** the arithmetic mean of the values in `ls`

```
> (mean '(1 2 3 4 5))
3
> (mean '(-10 0 10))
0
> (exact->inexact (mean '(1 2 3 4 5 150)))
27.5
```

#### procedure: `(median ls)`
**returns:** the median of `ls`

```
> (median '(1 2 3 4 5 6))
3.5
> (quantile '(1 2 3 4 5 6) 0.5 7))
3.5
```

#### procedure: `(mode ls)`
**returns:** a list with the values in `ls` that occur most frequently

```
> (mode '(1 1 1 2 2 2))
(1 2)
> (mode '(1 2 3 3 4 4 4))
(4)
```

#### procedure: `(quantile ls p type)`
**returns:** the sample quantile of the values in `ls` corresponding to the given probability, `p`, and `type`

The quantile function follows [Hyndman and Fan 1996](https://www.jstor.org/stable/2684934) who recommend type 8. The [default in R](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/quantile.html) is type 7.

```
> (quantile '(1 2 3 4 5 6) 0.5 1)
3
> (quantile '(1 2 3 4 5 6) 0.5 4)
3.0
> (quantile '(1 2 3 4 5 6) 0.5 8)
3.5
> (quantile '(1 2 3 4 5 6) 0.025 7)
1.125
```

#### procedure: `(range ls)`
**returns:** a pair containing the minimum and maximum values of `ls`

```
> (range '(1 2 3 4 5))
(1 . 5)
> (range '(-10 -7 3 -99 100))
(-99 . 100)
```

#### procedure: `(skewness ls)`
**returns:** the skewness of the values in `ls`

```
> (skewness '(1 2 3 4 5))
0.0
> (skewness '(1 2 2 3 3 3 4 4 4 4))
-0.6
```

#### procedure: `(standard-deviation ls)`
**returns:** the standard deviation of the values in `ls`

```
> (standard-deviation '(0 1 2 3 4 5))
1.8708286933869707
> (sqrt (variance '(0 1 2 3 4 5)))
1.8708286933869707
```

#### procedure: `(unique ls)`
**returns:** a sorted list of the unique values in `ls`

```
> (unique '(0.5 #e0.5 1/2 1 1 1 5.2))
(1/2 1 5.2)
> (unique '(0 0 0 1 1 1 2))
(0 1 2)
```

#### procedure: `(variance ls)`
**returns:** the sample variance of the values in `ls` based on [Welford's algorithm](https://www.johndcook.com/blog/standard_deviation/)

```
> (variance '(1 10 100 1000))
233840.25
> (variance '(0 1 2 3 4 5))
3.5
```

#### procedure: `(weighted-mean ls weights)`
**returns:** the arithmetic mean of the values in `ls` weighted by the values in `weights`

```
> (weighted-mean '(1 2 3 4 5) '(5 4 3 2 1))
7/3
> (weighted-mean '(1 2 3 4 5) '(2 2 2 2 2))
3
> (mean '(1 2 3 4 5))
3
> (weighted-mean '(1 2 3 4 5) '(2 0 2 2 2))
13/4
> (mean '(1 3 4 5))
13/4
```

## Read and Write CSV Files

There is nothing sophisticated about this approach to reading CSV files. For all files, `read-csv` produces a list of lists of strings.
There is no attempt to convert strings to numbers or other objects. The CSV file needs to be rectangular, i.e., every row must have the
same number of columns.

Import only the CSV procedures: `(import (chez-stats csv))`

#### procedure: `(preview-csv path rows)`
**returns:** a list of lists where each sub-list is one row in the CSV file, `path`, up to the number of `rows`

```
# example requires that you first run code for (write-csv) below
> (preview-csv "example.csv" 2)
(("col1" "col2" "col3" "col4")
  ("10.02" "A" "\"1,000\"" "\"Glen \"Big Baby\" Davis\""))
```

#### procedure: `(read-csv path)`
**returns:** a list of lists where each sub-list is one row in the CSV file, `path`

```
# example requires that you first run code for (write-csv) below
> (read-csv "example.csv")
(("col1" "col2" "col3" "col4")
  ("10.02" "A" "\"1,000\"" "\"Glen \"Big Baby\" Davis\"")
  ("0.3333333333333333" "B" "1000" "\"Earvin \"Magic\" Johnson\""))
```

#### procedure: `(write-csv ls path overwrite)`
**returns:** writes a list of lists as a CSV file to `path`; if file exists at `path`, operation will fail unless `overwrite` is `#t`

```
> (define example-list (list
                        (list "col1" "col2" "col3" "col4")
                        (list 10.02 #\A "1,000" "Glen \"Big Baby\" Davis")
                        (list 1/3 #\B "1000" "Earvin \"Magic\" Johnson")))
> (display example-list)
((col1 col2 col3 col4) (10.02 A 1,000 Glen "Big Baby" Davis) (1/3 B 1000 Earvin "Magic" Johnson))
> (write-csv example-list "example.csv" #f)
```

## Generate Random Variates

Import only the random variate procedures: `(import (chez-stats random-variates))`

#### procedure: `(random-bernoulli n p)`
**returns:** a list of `n` numbers from a Bernoulli distribution with probability `p`

```
> (random-bernoulli 25 0.1)
(0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 1 0 0 0 0)
> (random-bernoulli 25 0.9)
(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1)
```

#### procedure: `(random-beta n a b)`
**returns:** a list of `n` numbers from a beta distribution with shape parameters `a` and `b`

```
> (random-beta 10 1 1)
(0.1608787838443958 0.13619509140081779 0.9834731616787276 0.5743357684870621
  0.8637598266739267 0.6942190873522962 0.645854411263454
  0.41051274063753873 0.668801118029433 0.7873753287243728)
> (map round (random-beta 10 0.01 1))
(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
> (map round (random-beta 10 1 0.01))
(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)
```

#### procedure: `(random-beta-binomial n trials p dispersion)`
**returns:** a list of `n` successes out the number of `trials` from a binomial distribution where probability of success `p` is drawn from a beta distribution with shape parameters derived from `p` and `dispersion`

```
> (random-beta-binomial 25 10 0.5 1.001)
(5 3 7 6 6 4 4 7 2 4 5 2 6 8 5 5 8 6 4 4 3 3 4 5 5)
> (random-beta-binomial 25 10 0.5 9)
(10 10 0 10 10 0 10 1 2 2 0 0 10 0 8 2 10 10 10 0 10 10 0 0 0)
> (exact->inexact (mean (random-beta-binomial 1e5 10 0.5 1.001)))
5.00051
> (exact->inexact (mean (random-binomial 1e5 10 0.5)))
5.0002
> (variance (random-beta-binomial 1e5 10 0.5 1.001))
2.5040549180491807
> (variance (random-binomial 1e5 10 0.5))
2.5000544964449647
> (exact->inexact (mean (random-beta-binomial 1e5 10 0.5 9)))
4.98816
> (variance (random-beta-binomial 1e5 10 0.5 9))
22.50210069290693
```

#### procedure: `(random-binomial n trials p)`
**returns:** a list of `n` successes out of the number of `trials` from a binomial distribution with probability `p`

```
> (random-binomial 25 10 0.5)
(7 5 4 4 6 5 7 5 5 3 5 4 6 7 4 7 3 4 3 8 5 5 7 6 8)
> (random-binomial 25 100 0.5)
(57 47 49 52 48 55 48 49 60 46 61 49 48 46 53 53 57 57 47 58 44 53 57 54 47)
> (random-binomial 25 1 0.5)
(1 0 0 0 1 0 1 0 0 1 0 0 1 0 1 0 1 1 1 0 1 0 1 0 0)
 ```

#### procedure: `(random-exponential n mu)`
**returns:** a list of `n` numbers from an exponential distribution with mean `mu`

```
> (random-exponential 10 100)
(35.8597072715694 104.1153422246636 61.130577404212985 74.51016205480595
  28.757623000674293 69.03367489570623 1.9901391744468298
  32.16039857943056 16.818138818937218 38.53838415351449)
```

#### procedure: `(random-gamma n shape rate)`
**returns:** a list of `n` numbers from an gamma distribution with `shape` and `rate` parameters

```
> (random-gamma 10 1 1)
(0.18951484852194106 0.2863156678119879 0.5263915675137112 1.774829314438009
  0.5811076220295317 1.6993576614297963 1.243626305131102
  1.17084207353143 0.2806255087837392 0.2860118057459071)
> (mean (random-gamma 1e5 5 5))
0.9995798340045534
> (mean (random-gamma 1e5 10 10))
0.9989805807416875
```

#### procedure: `(random-geometric n p)`
**returns:** a list of `n` numbers from a geometric distribution with probability `p`

The probability distribution of the number of Bernoulli trials needed to get one success, supported on the set { 1, 2, 3, ... } (see [Wikipedia](https://en.wikipedia.org/wiki/Geometric_distribution)). Note, `rgeom` in R uses the other version of the geometric distribution described on the Wikipedia page.

```
> (random-geometric 25 0.2)
(3.0 3.0 6.0 13.0 1.0 4.0 3.0 7.0 3.0 2.0 9.0 1.0 2.0 1.0 6.0 4.0 4.0 1.0 10.0 1.0 2.0 1.0 3.0 4.0 21.0)
> (random-geometric 25 0.8)
(1.0 2.0 1.0 1.0 1.0 2.0 2.0 1.0 1.0 1.0 1.0 1.0 1.0 2.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)
```

#### procedure: `(random-lognormal n mulog sdlog)`
**returns:** a list of `n` numbers from a log normal distribution; `mulog` and `sdlog` are the mean and standard deviation of the distribution on the log scale

```
> (random-lognormal 10 0.5 0.5)
(1.7753306883641662 0.9327713859192857 1.9962785771068654 3.679668320402791
  1.7400539336159713 3.171605081866387 0.39297081354878666
  1.7114423881850356 1.455971890328584 1.1655978691542683)
> (random-lognormal 10 0.5 2)
(0.2969164016733139 0.2365155761845435 5.046466120321887 1.130781900343789
  6.369004081277258 0.3286021295817909 0.08029195517816963
  4.048941125846343 0.13855459327532965 8.481507871950905)
> (mean (map log (random-lognormal 1e5 0.5 0.5)))
0.5010480725785834
> (standard-deviation (map log (random-lognormal 1e5 0.5 0.5)))
0.4995377386643435
```

#### procedure: `(random-negative-binomial n trials p)`
**returns:** a list of `n` successes from a negative binomial distribution with target number of successful `trials` with probability `p` of success

```
> (random-negative-binomial 25 11.5 0.5)
(14 11 8 5 9 23 12 4 11 12 15 8 14 12 12 11 14 15 14 11 12 17 12 12 10)
> (exact->inexact (mean (random-negative-binomial 1e5 7 0.5)))
6.99912
> (exact->inexact (mean (random-poisson 1e5 7)))
7.00023
```

#### procedure: `(random-normal n mu sd)`
**returns:** a list of `n` numbers from a normal distribution with mean `mu` and standard deviation `sd`

```
> (random-normal 10 100 0.1)
(99.84784484903868 99.89799008859833 100.06300994079052 100.00286968094662
  99.89627748598733 99.9999359828298 100.02587497251288
  100.1098673482077 100.09046451628667 99.98730494625542)
> (random-normal 10 100 100)
(32.583830587945286 -120.83252735310398 242.64250642313553 92.39493192862878
  164.39808748159305 22.8058534483159 158.33535128554186
  -30.757726972313066 132.37810774263465 145.9341465922021)
```

#### procedure: `(random-pareto n shape)`
**returns:** a list of `n` numbers from a Pareto distribution with `shape` parameter

```
> (random-pareto 10 1)
(1.1832574208131592 1.1148930254197593 4.195463431627 1.3200617807665502
  1.9859628002254515 1.2586921428918592 1.7628680791986209
  2.040914305978817 1.7318113216158157 1.3009663204194946)
> (random-pareto 10 3)
(1.4037062644512017 1.1054698023959297 1.0022192639936547 2.5126775158365344
  1.6214825174549339 1.2489834137377076 1.3914657545229647
  2.389540116143122 1.9472706245609315 1.591010960196833)
```

#### procedure: `(random-poisson n mu)`
**returns:** a list of `n` integers from a Poisson distribution with mean and variance `mu`

```
> (random-poisson 25 10)
(8 12 16 8 15 6 12 12 12 6 8 10 13 15 12 12 8 12 8 10 10 11 12 13 8)
> (random-poisson 25 100)
(102 94 107 102 106 100 99 102 94 88 85 103 96 92 110 105 83 87 109 84 98 105 83 107 111)
```

#### procedure: `(random-uniform n mn mx)`
**returns:** a list of `n` numbers from a uniform distribution with mininum `mn` and maximum `mx`

```
> (random-uniform 10 -100 100)
(-65.5058597140247 61.16064610295348 -2.6071638962549457 -53.230103242300466
  78.5740908243061 6.188190661434589 -62.80124237884732
  -75.50128468420634 16.438291149933804 -89.67898368495186)
> (apply min (random-uniform 1e5 -10 10))
-9.999928733983786
```
