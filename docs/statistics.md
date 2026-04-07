# Descriptive Statistics

## `(count-unique lst)`

**returns:** a list of pairs where the `car` and `cdr` of each pair are the unique values and counts, respectively, of the values in list `lst`

```scheme
> (count-unique '(1 2 3 4 2 1))
((1 . 2) (2 . 2) (3 . 1) (4 . 1))
> (count-unique '(1.1 1 2.2 2 1.1 1.1))
((1 . 1) (1.1 . 3) (2 . 1) (2.2 . 1))
> (count-unique '(0.5 1/2 #e0.5 1 1 2))
((1/2 . 3) (1 . 2) (2 . 1))
> (count-unique '("a" "b" "b" "a"))
Exception in (count-unique lst): at least one element of lst is not a real number
```

## `(correlation x y method)`

**returns:** correlation coefficient between values in lists `x` and `y`; methods available: `'pearson`, `'spearman`, `'kendall`

```scheme
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

## `(cumulative-sum lst)`

**returns:** a list that is the cumulative sum of the values in `lst`

```scheme
> (cumulative-sum '(1 2 3 4 5))
(1 3 6 10 15)
> (cumulative-sum '(5 4 3 2 1))
(5 9 12 14 15)
```

## `(diff lst)`

**returns:** list of differences (with lag of one) for all elements in `lst`

```scheme
> (diff '(1 3 7 13))
(2 4 6)
> (diff '(-10 20 -10))
(30 -30)
```

## `(interquartile-range lst [type])`

**returns:** the difference in the 0.25 and 0.75 sample quantiles of the values in `lst` corresponding to the given `type` (see [`quantile`](#quantile-lst-p-type) for more info on `type`)

```scheme
> (interquartile-range '(1 2 3 5 5))
3.3333333333333335
> (interquartile-range '(1 2 3 5 5) 1)
3
> (interquartile-range '(3 7 4 8 9 7) 9)
4.125
```

## `(kurtosis lst)`

**returns:** the kurtosis of the values in `lst`

```scheme
> (kurtosis '(-10 0 10))
3/2
> (kurtosis '(1 2 2 3 3 3))
51/25
```

## `(mean lst)`

**returns:** the arithmetic mean of the values in `lst`

```scheme
> (mean '(1 2 3 4 5))
3
> (mean '(-10 0 10))
0
> (exact->inexact (mean '(1 2 3 4 5 150)))
27.5
```

## `(median lst)`

**returns:** the median of `lst`

```scheme
> (median '(1 2 3 4 5 6))
3.5
> (quantile '(1 2 3 4 5 6) 0.5 7))
3.5
```

## `(mode lst)`

**returns:** a list with the values in `lst` that occur most frequently

```scheme
> (mode '(1 1 1 2 2 2))
(1 2)
> (mode '(1 2 3 3 4 4 4))
(4)
```

## `(quantile lst p [type])`

**returns:** the sample quantile of the values in `lst` corresponding to the given probability, `p`, and `type`

The quantile function follows [Hyndman and Fan 1996](https://www.jstor.org/stable/2684934) who recommend type 8, which is the default in `chez-stats`. The [default in R](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/quantile.html) is type 7.

```scheme
> (quantile '(1 2 3 4 5 6) 0.5 1)
3
> (quantile '(1 2 3 4 5 6) 0.5 4)
3.0
> (quantile '(1 2 3 4 5 6) 0.5 8)
3.5
> (quantile '(1 2 3 4 5 6) 0.025 7)
1.125
```

## `(rank lst [ties-method])`

**returns:** a list of the sample ranks for the values in `lst`; ties are handled by replacing ranks with `'min` (default), `'max`, or `'mean`

```scheme
> (rank '(50 20 50 40 30))
(4 1 4 3 2)
> (rank '(50 20 50 40 30) 'min)
(4 1 4 3 2)
> (rank '(50 20 50 40 30) 'max)
(5 1 5 3 2)
> (rank '(50 20 50 40 30) 'mean))
(9/2 1 9/2 3 2)
```

## `(rep n lst type)`

**returns:** the appended list formed by repeating the values in `lst` either `n` times or `n` times each; replicates behavior of [`rep`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/rep.html) in R

```scheme
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

## `(rle lst)`

**returns:** run length encoding as a list of pairs where the `car` and `cdr` of each pair are the values and lengths of the runs, respectively, for the values in list `lst`

```scheme
> (rle '(1 1 1 2 1 1))
((1 . 3) (2 . 1) (1 . 2))
> (rle '(2 2 2 5 3 3))
((2 . 3) (5 . 1) (3 . 2))
> (rle '("a" "b" "b" "a"))
Exception in (rle lst): at least one element of lst is not a real number
```

## `(sign x)`

**returns:** sign of `x`

```scheme
> (sign -3)
-1
> (sign 0)
0
> (sign 7)
1
```

## `(skewness lst)`

**returns:** the skewness of the values in `lst`

```scheme
> (skewness '(1 2 3 4 5))
0.0
> (skewness '(1 2 2 3 3 3 4 4 4 4))
-0.6
```

## `(standard-deviation lst)`

**returns:** the standard deviation of the values in `lst`

```scheme
> (standard-deviation '(0 1 2 3 4 5))
1.8708286933869707
> (sqrt (variance '(0 1 2 3 4 5)))
1.8708286933869707
```

## `(sum lst)`

**returns:** the sum of the values in `lst`

For performance, use `(apply + lst)` to avoid the overhead of the assertions in `(sum lst)`. `sum` provides the small convenience of allowing to sum across a list of boolean values (using `filter` and `length`).

```scheme
> (sum (iota 10))
45
> (apply + (iota 10))
45
> (sum '(#t #f #t #f #t))
3
> (length (filter (lambda (x) x) '(#t #f #t #f #t)))
3
```

## `(unique lst)`

**returns:** a sorted list of the unique values in `lst`

```scheme
> (unique '(0.5 #e0.5 1/2 1 1 1 5.2))
(1/2 1 5.2)
> (unique '(0 0 0 1 1 1 2))
(0 1 2)
```

## `(variance lst)`

**returns:** the sample variance of the values in `lst` based on [Welford's algorithm](https://www.johndcook.com/blog/standard_deviation/)

```scheme
> (variance '(1 10 100 1000))
233840.25
> (variance '(0 1 2 3 4 5))
3.5
```

## `(weighted-mean lst weights)`

**returns:** the arithmetic mean of the values in `lst` weighted by the values in `weights`

```scheme
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
