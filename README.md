# Chez Scheme Statistics Library

Read and write delimited text files, compute descriptive statistics, and generate random variates in Chez Scheme.

## Installation

### [Akku](https://akkuscm.org/packages/chez-stats/)

```
$ akku install chez-stats
```

For more information on getting started with [Akku](https://akkuscm.org/), see this [blog post](https://www.travishinkelman.com/posts/getting-started-with-akku-package-manager-for-scheme/).

### Manual

Clone or download this repository. Move `chez-stats.sls` and `chez-stats` folder from downloaded and unzipped folder to one of the directories listed when you run `(library-directories)` in Chez Scheme. For more information on installing Chez Scheme libraries, see blog posts for [macOS and Windows](https://www.travishinkelman.com/post/getting-started-with-chez-scheme-and-emacs/) or [Ubuntu](https://www.travishinkelman.com/post/getting-started-with-chez-scheme-and-emacs-ubuntu/).

## Import

```scheme
(import (chez-stats))
```

## Quick Example

```scheme
> (mean '(1 2 3 4 5))
3
> (correlation (iota 10) (iota 10) 'pearson)
1.0
> (random-sample 5 'normal 100 15)
(107.2 88.6 115.3 96.1 102.8)
```

## Documentation

Full API documentation is available at [https://hinkelman.github.io/chez-stats/](https://hinkelman.github.io/chez-stats/). 

## API Overview

### Descriptive Statistics

`count-unique`, `correlation`, `cumulative-sum`, `diff`, `interquartile-range`, `kurtosis`, `mean`, `median`, `mode`, `quantile`, `rank`, `rep`, `rle`, `sign`, `skewness`, `standard-deviation`, `sum`, `unique`, `variance`, `weighted-mean`

### Read and Write Delimited Text Files

`read-delim`, `write-delim`

### Generate Random Variates

`random-bernoulli`, `random-beta`, `random-beta-binomial`, `random-binomial`, `random-exponential`, `random-gamma`, `random-geometric`, `random-lognormal`, `random-multinomial`, `random-negative-binomial`, `random-normal`, `random-pareto`, `random-poisson`, `random-uniform`, `random-sample`, `repeat`, `shuffle`
