# Chez Scheme Statistics Library

Work in progress. Procedures for basic descriptive statistics and generating random variates. 

### Descriptive Statistics  

[`(count ls)`](#procedure-count-ls)  
[`(cumulative-sum ls)`](#procedure-cumulative-sum-ls)  
[`(ecdf ls)`](#procedure-ecdf-ls)  
[`(interquartile-range ls type)`](#procedure-interquartile-range-ls-type)  
[`(mean ls)`](#procdure-mean-ls)  
[`(median ls)`](#procedure-median-ls)  
[`(mode ls)`](#procedure-mode-ls)  
[`(quantile ls p type)`](#procedure-quantile-ls-p-type)  
[`(range ls)`](#procedure-range-ls)  
[`(standard-deviation ls)`](#procedure-standard-deviation-ls)  
[`(unique ls)`](#procedure-unique-ls)  
[`(variance ls)`](#procedure-variance-ls)  
[`(weighted-mean ls weights)`](#procedure-weighted-mean-ls-weights)

### Generating Random Variates

[`(random-bernoulli n p)`](#procedure-random-bernoulli-n-p)  
[`(random-binomial n trials p)`](#procedure-random-binomial-n-trials-p)  
[`(random-exponential n mu)`](#procedure-random-exponential-n-mu)  
[`(random-normal n mu sd)`](#procedure-random-normal-n-mu-sd)  
[`(random-poisson n mu)`](#procedure-random-poisson-n-mu)

### Descriptive Statistics

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
Exception in (count): all elements of list must be real numbers; with irritant ("a" "b" "b" "a")
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

### Generating Random Variates

#### procedure: `(random-bernoulli n p)`
**returns:** a list of `n` numbers from a Bernoulli distribution with probability `p`

```
> (random-bernoulli 25 0.1)
(0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 1 0 0 0 0)
> (random-bernoulli 25 0.9)
(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1)
```

#### procedure: `(random-binomial n trials p)`
**returns:** a list of `n` successes out of the specified number of `trials` from a binomial distribution with probability `p`

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

#### procedure: `(random-normal n mu sd)`
**returns:** a list of `n` integers from a normal distribution with mean `mu` and standard deviation `sd`

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

#### procedure: `(random-poisson n mu)`
**returns:** a list of `n` integers from a Poisson distribution with mean and variance `mu`

```
> (random-poisson 25 10)
(8 12 16 8 15 6 12 12 12 6 8 10 13 15 12 12 8 12 8 10 10 11 12 13 8)
> (random-poisson 25 100)
(102 94 107 102 106 100 99 102 94 88 85 103 96 92 110 105 83 87 109 84 98 105 83 107 111)
```
