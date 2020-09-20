# Chez Scheme Statistics Library

Read and write delimited text files, compute descriptive statistics, and generate random variates in Chez Scheme.

Related blog posts:  
[Writing a Chez Scheme library](https://www.travishinkelman.com/post/writing-chez-scheme-library/)  
[Reading and writing CSV files in Chez Scheme](https://www.travishinkelman.com/post/reading-writing-csv-files-chez-scheme/)  

## Installation

### Akku

```
$ akku install chez-stats
```

For more information on getting started with [Akku](https://akkuscm.org/), see this [blog post](https://www.travishinkelman.com/posts/getting-started-with-akku-package-manager-for-scheme/).

### Manual

Clone or download this repository. Move `chez-stats.sls` and `chez-stats` folder from downloaded and unzipped folder to one of the directories listed when you run `(library-directories)` in Chez Scheme. For more information on installing Chez Scheme libraries, see blog posts for [macOS and Windows](https://www.travishinkelman.com/post/getting-started-with-chez-scheme-and-emacs/) or [Ubuntu](https://www.travishinkelman.com/post/getting-started-with-chez-scheme-and-emacs-ubuntu/).

## Import 

`(import (chez-stats))`

## Table of Contents

### Descriptive Statistics  

[`(count-unique lst)`](#count-unique)  
[`(correlation x y method)`](#correlation)  
[`(cumulative-sum lst)`](#cumulative-sum)  
[`(diff lst)`](#diff)  
[`(kurtosis lst)`](#kurtosis)  
[`(mean lst)`](#mean)  
[`(median lst)`](#median)  
[`(mode lst)`](#mode)  
[`(quantile lst p type)`](#quantile)  
[`(rank lst ties-method)`](#rank)  
[`(rep n lst type)`](#rep)  
[`(rle lst)`](#rle)  
[`(sign x)`](#sign)  
[`(skewness lst)`](#skewness)  
[`(standard-deviation lst)`](#standard-deviation)  
[`(unique lst)`](#unique)  
[`(variance lst)`](#variance)  
[`(weighted-mean lst weights)`](#weighted-mean)

### Read and Write Delimited Text Files

[`(read-delim path sep-char max-rows)`](#read-delim)  
[`(write-delim lst path sep-char overwrite)`](#write-delim)  

### Generate Random Variates

[`(random-bernoulli p)`](#random-bernoulli)  
[`(random-beta a b)`](#random-beta)  
[`(random-beta-binomial trials p dispersion)`](#random-beta-binomial)  
[`(random-binomial trials p)`](#random-binomial)  
[`(random-exponential mu)`](#random-exponential)  
[`(random-gamma shape rate)`](#random-gamma)  
[`(random-geometric p)`](#random-geometric)  
[`(random-lognormal mulog sdlog)`](#random-lognormal)  
[`(random-multinomial trials ps)`](#random-multinomial)  
[`(random-negative-binomial trials p)`](#random-negative-binomial)  
[`(random-normal mu sd)`](#random-normal)  
[`(random-pareto shape)`](#random-pareto)  
[`(random-poisson mu)`](#random-poisson)  
[`(random-uniform mn mx)`](#random-uniform)  
[`(repeat n thunk)`](#repeat)  

## Descriptive Statistics

#### <a name="count-unique"></a> procedure: `(count-unique lst)`
**returns:** a list of pairs where the `car` and `cdr` of each pair are the unique values and counts, respectively, of the values in list `lst`

```
> (count-unique '(1 2 3 4 2 1))
((1 . 2) (2 . 2) (3 . 1) (4 . 1))
> (count-unique '(1.1 1 2.2 2 1.1 1.1))
((1 . 1) (1.1 . 3) (2 . 1) (2.2 . 1))
> (count-unique '(0.5 1/2 #e0.5 1 1 2))
((1/2 . 3) (1 . 2) (2 . 1))
> (count-unique '("a" "b" "b" "a"))
Exception in (count-unique lst): at least one element of lst is not a real number
```

#### <a name="correlation"></a> procedure: `(correlation x y method)`
**returns:** correlation coefficient between values in lists `x` and `y`; methods available: `'pearson`, `'spearman`, `'kendall`

```
> (correlation (iota 10) (iota 10) 'pearson)
1.0
> (correlation (iota 10) (map - (iota 10)) 'pearson)
-1.0
> (correlation '(1 2 3 4) '(2 2.01 2.01 2) 'pearson)
0.0
> (define x '(86 97 99 100 101 103 106 110 112 113 86))
> (define y '(2 20 28 27 50 29 7 17 6 12 1))
> (correlation x y 'pearson)
0.15611738363791983
> (correlation x y 'spearman)
0.11389551189455129
> (correlation x y 'kendall)
0.07339758434175737
```

#### <a name="cumulative-sum"></a> procedure: `(cumulative-sum lst)`
**returns:** a list that is the cumulative sum of the values in `lst`

```
> (cumulative-sum '(1 2 3 4 5))
(1 3 6 10 15)
> (cumulative-sum '(5 4 3 2 1))
(5 9 12 14 15)
```

#### <a name="diff"></a> procedure: `(diff lst)`
**returns:** list of differences (with lag of one) for all elements in `lst`

```
> (diff '(1 3 7 13))
(2 4 6)
> (diff '(-10 20 -10))
(30 -30)
```

#### <a name="kurtosis"></a> procedure: `(kurtosis lst)`
**returns:** the kurtosis of the values in `lst`

```
> (kurtosis '(-10 0 10))
3/2
> (kurtosis '(1 2 2 3 3 3))
51/25
```

#### <a name="mean"></a> procedure: `(mean lst)`
**returns:** the arithmetic mean of the values in `lst`

```
> (mean '(1 2 3 4 5))
3
> (mean '(-10 0 10))
0
> (exact->inexact (mean '(1 2 3 4 5 150)))
27.5
```

#### <a name="median"></a> procedure: `(median lst)`
**returns:** the median of `lst`

```
> (median '(1 2 3 4 5 6))
3.5
> (quantile '(1 2 3 4 5 6) 0.5 7))
3.5
```

#### <a name="mode"></a> procedure: `(mode lst)`
**returns:** a list with the values in `lst` that occur most frequently

```
> (mode '(1 1 1 2 2 2))
(1 2)
> (mode '(1 2 3 3 4 4 4))
(4)
```

#### <a name="quantile"></a> procedure: `(quantile lst p type)`
**returns:** the sample quantile of the values in `lst` corresponding to the given probability, `p`, and `type`

The quantile function follows [Hyndman and Fan 1996](https://www.jstor.org/stable/2684934) who recommend type 8, which is the default in `chez-stats`. The [default in R](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/quantile.html) is type 7.

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

#### <a name="rank"></a> procedure: `(rank lst ties-method)`
**returns:** a list of the sample ranks for the values in `lst`; ties are handled by replacing ranks with `'min` (default), `'max`, or `'mean`

```
> (rank '(50 20 50 40 30))
(4 1 4 3 2)
> (rank '(50 20 50 40 30) 'min)
(4 1 4 3 2)
> (rank '(50 20 50 40 30) 'max)
(5 1 5 3 2)
> (rank '(50 20 50 40 30) 'mean))
(9/2 1 9/2 3 2)
```

#### <a name="rep"></a> procedure: `(rep n lst type)`
**returns:** the appended list formed by repeating the values in `lst` either `n` times or `n` times each; replicates behavior of [`rep`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/rep.html) in R

```
> (rep 3 '(1 2) 'times)
(1 2 1 2 1 2)
> (rep 3 '(1 2) 'each)
(1 1 1 2 2 2)
> (rep 3 '(a b) 'times)
(a b a b a b)
> (rep 3 '((1 2) (a b)) 'times)
((1 2) (a b) (1 2) (a b) (1 2) (a b))
> (rep 3 '((1 2) (a b)) 'each)
((1 2) (1 2) (1 2) (a b) (a b) (a b))
```

#### <a name="rle"></a> procedure: `(rle lst)`
**returns:** run length encoding as a list of pairs where the `car` and `cdr` of each pair are the values and lengths of the runs, respectively, for the values in list `lst`

```
> (rle '(1 1 1 2 1 1))
((1 . 3) (2 . 1) (1 . 2))
> (rle '(2 2 2 5 3 3))
((2 . 3) (5 . 1) (3 . 2))
> (rle '("a" "b" "b" "a"))
Exception in (rle lst): at least one element of lst is not a real number
```

#### <a name="sign"></a> procedure: `(sign x)`
**returns:** sign of `x`

```
> (sign '(-10 0 10))
(-1 0 1)
```

#### <a name="skewness"></a> procedure: `(skewness lst)`
**returns:** the skewness of the values in `lst`

```
> (skewness '(1 2 3 4 5))
0.0
> (skewness '(1 2 2 3 3 3 4 4 4 4))
-0.6
```

#### <a name="standard-deviation"></a> procedure: `(standard-deviation lst)`
**returns:** the standard deviation of the values in `lst`

```
> (standard-deviation '(0 1 2 3 4 5))
1.8708286933869707
> (sqrt (variance '(0 1 2 3 4 5)))
1.8708286933869707
```

#### <a name="unique"></a> procedure: `(unique lst)`
**returns:** a sorted list of the unique values in `lst`

```
> (unique '(0.5 #e0.5 1/2 1 1 1 5.2))
(1/2 1 5.2)
> (unique '(0 0 0 1 1 1 2))
(0 1 2)
```

#### <a name="variance"></a> procedure: `(variance lst)`
**returns:** the sample variance of the values in `lst` based on [Welford's algorithm](https://www.johndcook.com/blog/standard_deviation/)

```
> (variance '(1 10 100 1000))
233840.25
> (variance '(0 1 2 3 4 5))
3.5
```

#### <a name="weighted-mean"></a> procedure: `(weighted-mean lst weights)`
**returns:** the arithmetic mean of the values in `lst` weighted by the values in `weights`

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

## Read and Write Delimited Text Files

There is nothing sophisticated about this approach to reading delimited text files. For all files, `read-delimited` produces a list of lists of strings.
There is no attempt to convert strings to numbers or other objects. The file contents needs to be rectangular, i.e., every row must have the
same number of columns.

#### <a name="read-delim"></a> procedure: `(read-delim path sep-char max-rows)`
**returns:** a list of lists where each sub-list is one row in the file at `path`; `sep-char` and `max-rows` are optional and default to `#\,` and `+inf.0`, respectively 

#### <a name="write-delim"></a> procedure: `(write-delim lst path sep-char overwrite)`
**writes:** a list of lists `lst` as a delimited text file to `path`; `sep-char` and `overwrite` are optional and default to `#\,` and `#t`, respectively.


```
> (define example-list (list
                        (list "col1" "col2" "col3" "col4")
                        (list 10.02 #\A "1,000" "Glen \"Big Baby\" Davis")
                        (list 1/3 #\B "1000" "Earvin \"Magic\" Johnson")))
> (display example-list)
((col1 col2 col3 col4) (10.02 A 1,000 Glen "Big Baby" Davis) (1/3 B 1000 Earvin "Magic" Johnson))
> (write-delim example-list "example.csv")

> (read-delim "example.csv")
(("col1" "col2" "col3" "col4")
  ("10.02" "A" "\"1,000\"" "\"Glen \"Big Baby\" Davis\"")
  ("0.3333333333333333" "B" "1000" "\"Earvin \"Magic\" Johnson\""))
```


## Generate Random Variates

#### <a name="random-bernoulli"></a> procedure: `(random-bernoulli p)`
**returns:** a random variate from a Bernoulli distribution with probability `p`

```
> (repeat 25 (lambda () (random-bernoulli 0.1)))
(0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 1 0 0 0 0)
> (repeat 25 (lambda () (random-bernoulli 0.9)))
(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1)
```

#### <a name="random-beta"></a> procedure: `(random-beta a b)`
**returns:** a random variate from a beta distribution with shape parameters `a` and `b`

```
> (repeat 10 (lambda () (random-beta 1 1)))
(0.1608787838443958 0.13619509140081779 0.9834731616787276 0.5743357684870621
  0.8637598266739267 0.6942190873522962 0.645854411263454
  0.41051274063753873 0.668801118029433 0.7873753287243728)
> (map round (repeat 10 (lambda () (random-beta 0.01 1))))
(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
> (map round (repeat 10 (lambda () (random-beta 1 0.01))))
(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)
```

#### <a name="random-beta-binomial"></a> procedure: `(random-beta-binomial trials p dispersion)`
**returns:** a random number of successes out the number of `trials` from a binomial distribution where probability of success `p` is drawn from a beta distribution with shape parameters derived from `p` and `dispersion`

```
> (repeat 25 (lambda () (random-beta-binomial 10 0.5 1.001)))
(5 3 7 6 6 4 4 7 2 4 5 2 6 8 5 5 8 6 4 4 3 3 4 5 5)
> (repeat 25 (lambda () (random-beta-binomial 10 0.5 9)))
(10 10 0 10 10 0 10 1 2 2 0 0 10 0 8 2 10 10 10 0 10 10 0 0 0)
> (exact->inexact (mean (repeat 1e5 (lambda () (random-beta-binomial 10 0.5 1.001)))))
5.00051
> (exact->inexact (mean (repeat 1e5 (lambda () (random-binomial 10 0.5)))))
5.0002
> (variance (repeat 1e5 (lambda () (random-beta-binomial 10 0.5 1.001))))
2.5040549180491807
> (variance (repeat 1e5 (lambda () (random-binomial 10 0.5))))
2.5000544964449647
> (exact->inexact (mean (repeat 1e5 (lambda () (random-beta-binomial 10 0.5 9)))))
4.98816
> (variance (repeat 1e5 (lambda () (random-beta-binomial 10 0.5 9))))
22.50210069290693
```

#### <a name="random-binomial"></a> procedure: `(random-binomial trials p)`
**returns:** a random number of successes out of the number of `trials` from a binomial distribution with probability `p`

```
> (repeat 25 (lambda () (random-binomial 10 0.5)))
(7 5 4 4 6 5 7 5 5 3 5 4 6 7 4 7 3 4 3 8 5 5 7 6 8)
> (repeat 25 (lambda () (random-binomial 100 0.5)))
(57 47 49 52 48 55 48 49 60 46 61 49 48 46 53 53 57 57 47 58 44 53 57 54 47)
> (repeat 25 (lambda () (random-binomial 1 0.5)))
(1 0 0 0 1 0 1 0 0 1 0 0 1 0 1 0 1 1 1 0 1 0 1 0 0)
 ```

#### <a name="random-exponential"></a> procedure: `(random-exponential mu)`
**returns:** a random variate from an exponential distribution with mean `mu`

```
> (repeat 10 (lambda () (random-exponential 100)))
(35.8597072715694 104.1153422246636 61.130577404212985 74.51016205480595
  28.757623000674293 69.03367489570623 1.9901391744468298
  32.16039857943056 16.818138818937218 38.53838415351449)
```

#### <a name="random-gamma"></a> procedure: `(random-gamma shape rate)`
**returns:** a random variate from an gamma distribution with `shape` and `rate` parameters

```
> (repeat 10 (lambda () (random-gamma 1 1)))
(0.18951484852194106 0.2863156678119879 0.5263915675137112 1.774829314438009
  0.5811076220295317 1.6993576614297963 1.243626305131102
  1.17084207353143 0.2806255087837392 0.2860118057459071)
> (mean (repeat 1e5 (lambda () (random-gamma 5 5))))
0.9995798340045534
> (mean (repeat 1e5 (lambda () (random-gamma 10 10))))
0.9989805807416875
```

#### <a name="random-geometric"></a> procedure: `(random-geometric p)`
**returns:** a random variate from a geometric distribution with probability `p`

The probability distribution of the number of Bernoulli trials needed to get one success, supported on the set { 1, 2, 3, ... } (see [Wikipedia](https://en.wikipedia.org/wiki/Geometric_distribution)). Note, `rgeom` in R uses the other version of the geometric distribution described on the Wikipedia page.

```
> (repeat 25 (lambda () (random-geometric 0.2)))
(3.0 3.0 6.0 13.0 1.0 4.0 3.0 7.0 3.0 2.0 9.0 1.0 2.0 1.0 6.0 4.0 4.0 1.0 10.0 1.0 2.0 1.0 3.0 4.0 21.0)
> (repeat 25 (lambda () (random-geometric 5 0.8)))
(1.0 2.0 1.0 1.0 1.0 2.0 2.0 1.0 1.0 1.0 1.0 1.0 1.0 2.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)
```

#### <a name="random-lognormal"></a> procedure: `(random-lognormal mulog sdlog)`
**returns:** a random variate from a lognormal distribution; `mulog` and `sdlog` are the mean and standard deviation of the distribution on the log scale

```
> (repeat 10 (lambda () (random-lognormal 0.5 0.5))
(1.7753306883641662 0.9327713859192857 1.9962785771068654 3.679668320402791
  1.7400539336159713 3.171605081866387 0.39297081354878666
  1.7114423881850356 1.455971890328584 1.1655978691542683)
> (repeat 10 (lambda () (random-lognormal 0.5 2))
(0.2969164016733139 0.2365155761845435 5.046466120321887 1.130781900343789
  6.369004081277258 0.3286021295817909 0.08029195517816963
  4.048941125846343 0.13855459327532965 8.481507871950905)
> (mean (map log (repeat 1e5 (lambda () (random-lognormal 0.5 0.5)))))
0.5010480725785834
> (standard-deviation (map log (repeat 1e5 (lambda () (random-lognormal 0.5 0.5)))))
0.4995377386643435
```

#### <a name="random-multinomial"></a> procedure: `(random-multinomial trials ps)`
**returns:** a random number of successes from a multinomial distribution that sums to `trials` and is the same length as the list of the probability `ps` of success; if necessary, `ps` is rescaled to sum to one

```
> (random-multinomial 10 '(0.01 0.5 0.49))
(0 7 3)
> (random-multinomial 100 '(0.01 0.5 0.49))
(2 51 47)
> (random-multinomial 100 '(1 50 49))
(2 45 53)
> (map (lambda (x) (/ x 1e5)) (random-multinomial 1e5 '(0.01 0.5 0.49)))
(0.01016 0.50004 0.4898)
```

#### <a name="random-negative-binomial"></a> procedure: `(random-negative-binomial trials p)`
**returns:** a random variate from a negative binomial distribution with target number of successful `trials` with probability `p` of success

```
> (repeat 25 (lambda () (random-negative-binomial 11.5 0.5)))
(14 11 8 5 9 23 12 4 11 12 15 8 14 12 12 11 14 15 14 11 12 17 12 12 10)
> (exact->inexact (mean (repeat 1e5 (lambda () (random-negative-binomial 7 0.5)))))
6.99912
> (exact->inexact (mean (repeat 1e5 (lambda () (random-poisson 7)))))
7.00023
```

#### <a name="random-normal"></a> procedure: `(random-normal mu sd)`
**returns:** a random variate from a normal distribution with mean `mu` and standard deviation `sd`; `mu` and `sd` are optional and default to `0` and `1`, respectively 

```
> (random-normal)
0.619596161566232
> (repeat 10 (lambda () (random-normal 100 0.1)))
(99.84784484903868 99.89799008859833 100.06300994079052 100.00286968094662
  99.89627748598733 99.9999359828298 100.02587497251288
  100.1098673482077 100.09046451628667 99.98730494625542)
> (repeat 10 (lambda () (random-normal 100 100)))
(32.583830587945286 -120.83252735310398 242.64250642313553 92.39493192862878
  164.39808748159305 22.8058534483159 158.33535128554186
  -30.757726972313066 132.37810774263465 145.9341465922021)
```

#### <a name="random-pareto"></a> procedure: `(random-pareto shape)`
**returns:** a random variate from a Pareto distribution with `shape` parameter

```
> (repeat 10 (lambda () (random-pareto 1)))
(1.1832574208131592 1.1148930254197593 4.195463431627 1.3200617807665502
  1.9859628002254515 1.2586921428918592 1.7628680791986209
  2.040914305978817 1.7318113216158157 1.3009663204194946)
> (repeat 10 (lambda () (random-pareto 3)))
(1.4037062644512017 1.1054698023959297 1.0022192639936547 2.5126775158365344
  1.6214825174549339 1.2489834137377076 1.3914657545229647
  2.389540116143122 1.9472706245609315 1.591010960196833)
```

#### <a name="random-poisson"></a> procedure: `(random-poisson mu)`
**returns:** a random variate from a Poisson distribution with mean and variance `mu`

```
> (repeat 25 (lambda () (random-poisson 10)))
(8 12 16 8 15 6 12 12 12 6 8 10 13 15 12 12 8 12 8 10 10 11 12 13 8)
> (repeat 25 (lambda () (random-poisson 100)))
(102 94 107 102 106 100 99 102 94 88 85 103 96 92 110 105 83 87 109 84 98 105 83 107 111)
```

#### <a name="random-uniform"></a> procedure: `(random-uniform mn mx)`
**returns:** a random variate from a uniform distribution with mininum `mn` and maximum `mx`

```
> (repeat 10 (lambda () (random-uniform -100 100)))
(-65.5058597140247 61.16064610295348 -2.6071638962549457 -53.230103242300466
  78.5740908243061 6.188190661434589 -62.80124237884732
  -75.50128468420634 16.438291149933804 -89.67898368495186)
> (apply min (repeat 1e5 (lambda () (random-uniform -10 10))))
-9.999928733983786
```

#### <a name="repeat"></a> procedure: `(repeat n thunk)`
**returns:** a list of `n` return values from repeatedly applying `thunk`

```
> (repeat 3 (lambda () "test"))
("test" "test" "test")
> (repeat 3 (let ([x 1]) (lambda () (add1 x))))
(2 2 2)
> (repeat 3 (lambda () (random-normal)))
(0.6050717276786769 0.3875905343441506 0.8670747717354842)
```
