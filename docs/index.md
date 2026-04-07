# Chez Scheme Statistics Library

Read and write delimited text files, compute descriptive statistics, and generate random variates in Chez Scheme.

Related blog posts:

- [Writing a Chez Scheme library](https://www.travishinkelman.com/post/writing-chez-scheme-library/)
- [Reading and writing CSV files in Chez Scheme](https://www.travishinkelman.com/post/reading-writing-csv-files-chez-scheme/)

## Installation

### Akku

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
