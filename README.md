# Chez Scheme Statistics Library

### [Descriptive Statistics](#descriptive-statistics)
* [count](#procedure-count-ls)
* [cumulative-sum](#cumulative-sum)
* [ecdf](#ecdf)
* [interquartile-range](#interquartile-range)
* [mean](#mean)
* [median](#median)
* [mode](#mode)
* [quantile](#quantile)
* [range](#range)
* [standard-deviation](#standard-deviation)
* [unique](#unique)
* [variance](#variance)
* [weighted-mean](#procedure-weighted-mean-ls-weights)

### [Generating Random Variates](#generating-random-deviates)

* [random-bernoulli](#random-bernoulli)
* [random-binomial](#random-binomial)
* [random-exponential](#random-exponential)
* [random-poisson](#random-poisson)

### Descriptive Statistics

#### count
**procedure:** `(count ls)`  
**returns:** a list containing a sorted list of the unique values in the input list and a list of counts that correspond to the unique values

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
#### cumulative-sum
#### ecdf
#### interquartile-range
#### mean
#### median
#### mode
#### quantile
#### range
#### standard-deviation
#### unique
#### variance
#### weighted-mean
**procedure:** `(weighted-mean ls weights)`  

### Generating Random Variates

#### random-bernoulli
#### random-binomial
#### random-exponential
#### random-poisson
