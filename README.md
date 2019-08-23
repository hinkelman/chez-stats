# Chez Scheme Statistics Library

### [Descriptive Statistics](#descriptive-statistics)  
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

### [Generating Random Variates](#generating-random-deviates)

[`(random-bernoulli n p)`](#procedure-random-bernoulli-n-p)  
[`(random-binomial n trials p)`](#procedure-random-binomial-n-trials-p)  
[`(random-exponential n mu)`](#procedure-random-exponential-n-mu)  
[`(random-poisson n mu)`](#procedure-random-poisson-n-mu)

### Descriptive Statistics

#### procedure: `(count ls)`  
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
#### procedure: `(cumulative-sum ls)`  
**returns:**

#### procedure: `(ecdf ls)`  
**returns:**

#### procedure: `(interquartile-range ls type)`  
**returns:**

#### procedure: `(mean ls)`  
**returns:**

#### procedure: `(median ls)`  
**returns:**

#### procedure: `(mode ls)`  
**returns:**

#### procedure: `(quantile ls p type)`  
**returns:**

#### procedure: `(range ls)`  
**returns:**

#### procedure: `(standard-deviation ls)`  
**returns:**

#### procedure: `(unique ls)`  
**returns:**

#### procedure: `(variance ls)`  
**returns:**

#### procedure: `(weighted-mean ls weights)`  
**returns:**

### Generating Random Variates

#### procedure: `(random-bernoulli n p)`  
**returns:**

#### procedure: `(random-binomial n trials p)`  
**returns:**

#### procedure: `(random-exponential n mu)`  
**returns:**

#### procedure: `(random-poisson n mu)`  
**returns:**
