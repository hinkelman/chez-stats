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
[`(interquartile-range lst [type])`](#iqr)  
[`(kurtosis lst)`](#kurtosis)  
[`(mean lst)`](#mean)  
[`(median lst)`](#median)  
[`(mode lst)`](#mode)  
[`(quantile lst p [type])`](#quantile)  
[`(rank lst [ties-method])`](#rank)  
[`(rep n lst type)`](#rep)  
[`(rle lst)`](#rle)  
[`(sign x)`](#sign)  
[`(skewness lst)`](#skewness)  
[`(standard-deviation lst)`](#standard-deviation)  
[`(sum lst)`](#sum)  
[`(unique lst)`](#unique)  
[`(variance lst)`](#variance)  
[`(weighted-mean lst weights)`](#weighted-mean)

### Read and Write Delimited Text Files

[`(read-delim path [sep-char max-rows])`](#read-delim)  
[`(write-delim lst path [sep-char overwrite])`](#write-delim)  

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
[`(random-normal [mu sd])`](#random-normal)  
[`(random-pareto shape)`](#random-pareto)  
[`(random-poisson mu)`](#random-poisson)  
[`(random-uniform mn mx)`](#random-uniform)  
[`(random-sample n dist . args)`](#random-sample)  
[`(repeat n thunk)`](#repeat)  
[`(shuffle lst)`](#shuffle)  

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

#### <a name="iqr"></a> procedure: `(interquartile-range lst [type])`
**returns:** the difference in the 0.25 and 0.75 sample quantiles of the values in `lst` corresponding to the given `type` (see [`quantile`](#quantile) for more info on `type`)

```
> (interquartile-range '(1 2 3 5 5))
3.3333333333333335
> (interquartile-range '(1 2 3 5 5) 1)
3
> (interquartile-range '(3 7 4 8 9 7) 9)
4.125
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

#### <a name="quantile"></a> procedure: `(quantile lst p [type])`
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

#### <a name="rank"></a> procedure: `(rank lst [ties-method])`
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
> (sign -3)
-1
> (sign 0)
0
> (sign 7)
1
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

#### <a name="sum"></a> procedure: `(sum lst)`
**returns:** the sum of the values in `lst`

For performance, use `(apply + lst)` to avoid the overhead of the assertions in `(sum lst)`. `sum` provides the small convenience of allowing to sum across a list of boolean values (using `filter` and `length`).

```
> (sum (iota 10))
45
> (apply + (iota 10))
45
> (sum '(#t #f #t #f #t))
3
> (length (filter (lambda (x) x) '(#t #f #t #f #t)))
3
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
same number of columns. For an alternative approach, see [chez-csv](https://akkuscm.org/packages/chez-csv/).

#### <a name="read-delim"></a> procedure: `(read-delim path [sep-char max-rows])`
**returns:** a list of lists where each sub-list is one row in the file at `path`; `sep-char` and `max-rows` are optional and default to `#\,` and `+inf.0`, respectively 

#### <a name="write-delim"></a> procedure: `(write-delim lst path [sep-char overwrite])`
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
> (random-bernoulli 0.5)
1

> (random-sample 25 'bernoulli 0.1)
(0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 1 0 0 0 0 0 0 1 0)

> (random-sample 25 'bernoulli 0.9)
(1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1)
```

#### <a name="random-beta"></a> procedure: `(random-beta a b)`
**returns:** a random variate from a beta distribution with shape parameters `a` and `b`

```
> (random-beta 1 1)
0.25063122372933117

> (random-sample 10 'beta 1 1)
(0.7749958332382194 0.18097677722657585 0.9527440460335397 0.20598935606180452
  0.2655579174397114 0.9052058525283536 0.6320962468544247
  0.2407987720530186 0.777592073561739 0.42288166542693445)
  
> (map round (random-sample 10 'beta 0.01 1))
(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)

> (map round (random-sample 10 'beta 1 0.01))
(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)
```

#### <a name="random-beta-binomial"></a> procedure: `(random-beta-binomial trials p dispersion)`
**returns:** a random number of successes out the number of `trials` from a binomial distribution where probability of success `p` is drawn from a beta distribution with shape parameters derived from `p` and `dispersion`

```
> (random-beta-binomial 10 0.5 1.001)
5

> (random-sample 25 'beta-binomial 10 0.5 1.001)
(5 3 5 4 7 4 7 7 6 8 3 6 8 6 6 3 4 4 4 5 6 6 6 4 7)

> (random-sample 25 'beta-binomial 10 0.5 9)
(10 10 8 10 10 0 10 10 10 2 0 0 0 0 0 10 0 10 0 10 0 10 0 9
 10)
 
> (exact->inexact (mean (random-sample 1e5 'beta-binomial 10 0.5 1.001)))
4.99226

> (exact->inexact (mean (random-sample 1e5 'binomial 10 0.5)))
5.00106

> (exact->inexact (variance (random-sample 1e5 'beta-binomial 10 0.5 1.001)))
2.537116250762508

> (exact->inexact (variance (random-sample 1e5 'binomial 10 0.5)))
2.5001250008500087

> (exact->inexact (mean (random-sample 1e5 'beta-binomial 10 0.5 9)))
5.02686

> (exact->inexact (variance (random-sample 1e5 'beta-binomial 10 0.5 9)))
22.435713834638346
```

#### <a name="random-binomial"></a> procedure: `(random-binomial trials p)`
**returns:** a random number of successes out of the number of `trials` from a binomial distribution with probability `p`

```
> (random-binomial 10 0.5)
7

> (random-sample 25 'binomial 10 0.5)
(4 5 5 5 4 3 3 8 6 6 4 4 3 6 4 5 5 6 5 7 3 5 5 6 7)

> (random-sample 25 'binomial 100 0.5)
(50 43 50 47 46 56 51 53 55 59 51 58 50 46 54 58 55 57 41 48
 49 52 48 59 48)
 
> (random-sample 25 'binomial 1 0.5)
(0 1 1 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 1 0 0 1 0 0 0)
 ```

#### <a name="random-exponential"></a> procedure: `(random-exponential mu)`
**returns:** a random variate from an exponential distribution with mean `mu`

```
> (random-exponential 100)
54.054072181088

> (random-sample 10 'exponential 100)
(69.82604616331902 95.39078920805312 74.27370394712197 57.01433441034123
  152.57293905279477 92.68182093388592 21.95720439860792
  41.301403304112675 33.67575708845525 48.97568758225251)
```

#### <a name="random-gamma"></a> procedure: `(random-gamma shape rate)`
**returns:** a random variate from an gamma distribution with `shape` and `rate` parameters

```
> (random-gamma 1 1)
0.16128004517131933

> (random-sample 10 'gamma 1 1)
(0.2222198507385751 0.03204293874599289 1.2167682582506516 1.0715520064471686
  1.2506633023543428 1.4094864757219174 1.5828612896128993
  0.9452679105067731 0.6589018522006892 0.08156568078150264)
  
> (mean (random-sample 1e5 'gamma 5 5))
1.000184208852648

> (mean (random-sample 1e5 'gamma 10 10))
0.9995142170518269
```

#### <a name="random-geometric"></a> procedure: `(random-geometric p)`
**returns:** a random variate from a geometric distribution with probability `p`

The probability distribution of the number of Bernoulli trials needed to get one success, supported on the set { 1, 2, 3, ... } (see [Wikipedia](https://en.wikipedia.org/wiki/Geometric_distribution)). Note, `rgeom` in R uses the other version of the geometric distribution described on the Wikipedia page.

```
> (random-geometric 0.2)
8.0

> (random-sample 25 'geometric 0.2)
(4.0 5.0 2.0 17.0 9.0 2.0 1.0 8.0 7.0 2.0 5.0 13.0 3.0 3.0
 6.0 2.0 2.0 10.0 1.0 7.0 4.0 2.0 5.0 1.0 14.0)
 
> (random-sample 25 'geometric 0.8)
(1.0 1.0 1.0 2.0 1.0 1.0 1.0 1.0 1.0 1.0 2.0 1.0 1.0 1.0 1.0
 1.0 2.0 1.0 1.0 2.0 2.0 1.0 1.0 1.0 1.0)
```

#### <a name="random-lognormal"></a> procedure: `(random-lognormal mulog sdlog)`
**returns:** a random variate from a lognormal distribution; `mulog` and `sdlog` are the mean and standard deviation of the distribution on the log scale

```
> (random-lognormal 0.5 0.5)
1.434059946345356

> (random-sample 10 'lognormal 0.5 2)
(5.1275956461587615 2.1450117488384204 0.7964065560019347 1.3730080969358056
  2.5365308856514055 4.636183661695536 0.4772493671851817
  4.483696248972149 29.007130022354175 0.5983414412697867)
  
> (mean (map log (random-sample 1e5 'lognormal 0.5 0.5)))
0.5020555578040999

> (standard-deviation (map log (random-sample 1e5 'lognormal 0.5 0.5)))
0.49924242619194836
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

> (random-sample 5 'multinomial 100 '(1 50 49))
((2 51 47) (1 48 51) (2 50 48) (0 47 53) (1 57 42))

> (map (lambda (x) (/ x 1e5)) (random-multinomial 1e5 '(0.01 0.5 0.49)))
(0.01016 0.50004 0.4898)
```

#### <a name="random-negative-binomial"></a> procedure: `(random-negative-binomial trials p)`
**returns:** a random variate from a negative binomial distribution with target number of successful `trials` with probability `p` of success

```
> (random-negative-binomial 11.5 0.5)
12

> (random-sample 25 'negative-binomial 11.5 0.5)
(12 9 3 14 16 5 13 7 7 8 11 7 10 14 7 13 5 18 11 7 17 13 7 9
 14)
 
> (exact->inexact (mean (random-sample 1e5 'negative-binomial 7 0.5)))
7.02671

> (exact->inexact (mean (random-sample 1e5 'poisson 7)))
7.00099
```

#### <a name="random-normal"></a> procedure: `(random-normal [mu sd])`
**returns:** a random variate from a normal distribution with mean `mu` and standard deviation `sd`; `mu` and `sd` are optional and default to `0` and `1`, respectively 

```
> (random-normal)
-1.073443722224577

> (random-sample 10 'normal)
(0.07504269802649746 -0.529337241978542 1.69813421585322 0.11271326169543866
  -0.07261733613433384 0.5685056161238756 -0.7043919930635121
  -0.019231353920430537 0.24463845886779126
  0.3829409082781564)
  
> (random-sample 10 'normal 100 0.1)
(100.14629198812324 99.92209566727179 100.08795620757246 99.7698733065516
  99.99709503218988 99.72647348087824 100.00981797327778
  100.02325765308501 99.82866338343638 99.7841803255381)
  
> (random-sample 10 'normal 100 100)
(-82.16644991668062 21.096014980927265 162.0817602665973 325.903839633812
  199.20300636050234 64.47078992212485 90.40622355827253
  81.42529215838913 124.1501278856605 -63.335050543523124)
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
> (random-poisson 10)
19

> (random-sample 20 'poisson 10)
(11 13 10 5 10 10 8 13 9 4 9 10 10 10 11 9 4 7 9 9)

> (random-sample 20 'poisson 100)
(105 99 96 105 114 103 94 105 102 118 106 117 111 105 107 106 109 120 106 74)
```

#### <a name="random-uniform"></a> procedure: `(random-uniform mn mx)`
**returns:** a random variate from a uniform distribution with minimum `mn` and maximum `mx`

```
> (random-uniform -100 100)
79.26451873291577

> (random-sample 10 'uniform -100 100)
(-8.255186335366545 23.02355866880434 -8.871540316004896 -44.802452342478325
  2.0827387754077478 31.704390108207235 -51.90255875734358
  79.19020558189484 4.61707910408937 64.60966334131024)
  
> (apply min (random-sample 1e5 'uniform -10 10))
-9.99973840034109
```

#### <a name="random-sample"></a> procedure: `(random-sample n dist . args)`
**returns:** a sample of `n` draws from the distribution `dist` with `args` used in matching procedure, e.g., `'uniform` as `dist` calls `random-uniform`

```
> (random-sample 10 'uniform -100 100)
(-8.255186335366545 23.02355866880434 -8.871540316004896 -44.802452342478325
  2.0827387754077478 31.704390108207235 -51.90255875734358
  79.19020558189484 4.61707910408937 64.60966334131024)
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

#### <a name="shuffle"></a> procedure: `(shuffle lst)`
**returns:** a randomly sorted list of the values in `lst` 

```
> (shuffle (iota 5))
(3 4 0 1 2)

> (shuffle (iota 5))
(2 0 4 3 1)

> (shuffle (iota 5))
(0 1 4 2 3)
```
