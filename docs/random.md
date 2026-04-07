# Generate Random Variates

## `(random-bernoulli p)`

**returns:** a random variate from a Bernoulli distribution with probability `p`

```scheme
> (random-bernoulli 0.5)
1

> (random-sample 25 'bernoulli 0.1)
(0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 1 0 0 0 0 0 0 1 0)

> (random-sample 25 'bernoulli 0.9)
(1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1)
```

## `(random-beta a b)`

**returns:** a random variate from a beta distribution with shape parameters `a` and `b`

```scheme
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

## `(random-beta-binomial trials p dispersion)`

**returns:** a random number of successes out the number of `trials` from a binomial distribution where probability of success `p` is drawn from a beta distribution with shape parameters derived from `p` and `dispersion`

```scheme
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

## `(random-binomial trials p)`

**returns:** a random number of successes out of the number of `trials` from a binomial distribution with probability `p`

```scheme
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

## `(random-exponential mu)`

**returns:** a random variate from an exponential distribution with mean `mu`

```scheme
> (random-exponential 100)
54.054072181088

> (random-sample 10 'exponential 100)
(69.82604616331902 95.39078920805312 74.27370394712197 57.01433441034123
  152.57293905279477 92.68182093388592 21.95720439860792
  41.301403304112675 33.67575708845525 48.97568758225251)
```

## `(random-gamma shape rate)`

**returns:** a random variate from a gamma distribution with `shape` and `rate` parameters

```scheme
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

## `(random-geometric p)`

**returns:** a random variate from a geometric distribution with probability `p`

The probability distribution of the number of Bernoulli trials needed to get one success, supported on the set { 1, 2, 3, ... } (see [Wikipedia](https://en.wikipedia.org/wiki/Geometric_distribution)). Note, `rgeom` in R uses the other version of the geometric distribution described on the Wikipedia page.

```scheme
> (random-geometric 0.2)
8.0

> (random-sample 25 'geometric 0.2)
(4.0 5.0 2.0 17.0 9.0 2.0 1.0 8.0 7.0 2.0 5.0 13.0 3.0 3.0
 6.0 2.0 2.0 10.0 1.0 7.0 4.0 2.0 5.0 1.0 14.0)

> (random-sample 25 'geometric 0.8)
(1.0 1.0 1.0 2.0 1.0 1.0 1.0 1.0 1.0 1.0 2.0 1.0 1.0 1.0 1.0
 1.0 2.0 1.0 1.0 2.0 2.0 1.0 1.0 1.0 1.0)
```

## `(random-lognormal mulog sdlog)`

**returns:** a random variate from a lognormal distribution; `mulog` and `sdlog` are the mean and standard deviation of the distribution on the log scale

```scheme
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

## `(random-multinomial trials ps)`

**returns:** a random number of successes from a multinomial distribution that sums to `trials` and is the same length as the list of the probability `ps` of success; if necessary, `ps` is rescaled to sum to one

```scheme
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

## `(random-negative-binomial trials p)`

**returns:** a random variate from a negative binomial distribution with target number of successful `trials` with probability `p` of success

```scheme
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

## `(random-normal [mu sd])`

**returns:** a random variate from a normal distribution with mean `mu` and standard deviation `sd`; `mu` and `sd` are optional and default to `0` and `1`, respectively

```scheme
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

## `(random-pareto shape)`

**returns:** a random variate from a Pareto distribution with `shape` parameter

```scheme
> (repeat 10 (lambda () (random-pareto 1)))
(1.1832574208131592 1.1148930254197593 4.195463431627 1.3200617807665502
  1.9859628002254515 1.2586921428918592 1.7628680791986209
  2.040914305978817 1.7318113216158157 1.3009663204194946)
> (repeat 10 (lambda () (random-pareto 3)))
(1.4037062644512017 1.1054698023959297 1.0022192639936547 2.5126775158365344
  1.6214825174549339 1.2489834137377076 1.3914657545229647
  2.389540116143122 1.9472706245609315 1.591010960196833)
```

## `(random-poisson mu)`

**returns:** a random variate from a Poisson distribution with mean and variance `mu`

```scheme
> (random-poisson 10)
19

> (random-sample 20 'poisson 10)
(11 13 10 5 10 10 8 13 9 4 9 10 10 10 11 9 4 7 9 9)

> (random-sample 20 'poisson 100)
(105 99 96 105 114 103 94 105 102 118 106 117 111 105 107 106 109 120 106 74)
```

## `(random-uniform mn mx)`

**returns:** a random variate from a uniform distribution with minimum `mn` and maximum `mx`

```scheme
> (random-uniform -100 100)
79.26451873291577

> (random-sample 10 'uniform -100 100)
(-8.255186335366545 23.02355866880434 -8.871540316004896 -44.802452342478325
  2.0827387754077478 31.704390108207235 -51.90255875734358
  79.19020558189484 4.61707910408937 64.60966334131024)

> (apply min (random-sample 1e5 'uniform -10 10))
-9.99973840034109
```

## `(random-sample n dist . args)`

**returns:** a sample of `n` draws from the distribution `dist` with `args` used in matching procedure, e.g., `'uniform` as `dist` calls `random-uniform`

```scheme
> (random-sample 10 'uniform -100 100)
(-8.255186335366545 23.02355866880434 -8.871540316004896 -44.802452342478325
  2.0827387754077478 31.704390108207235 -51.90255875734358
  79.19020558189484 4.61707910408937 64.60966334131024)
```

## `(repeat n thunk)`

**returns:** a list of `n` return values from repeatedly applying `thunk`

```scheme
> (repeat 3 (lambda () "test"))
("test" "test" "test")

> (repeat 3 (let ([x 1]) (lambda () (add1 x))))
(2 2 2)

> (repeat 3 (lambda () (random-normal)))
(0.6050717276786769 0.3875905343441506 0.8670747717354842)
```

## `(shuffle lst)`

**returns:** a randomly sorted list of the values in `lst`

```scheme
> (shuffle (iota 5))
(3 4 0 1 2)

> (shuffle (iota 5))
(2 0 4 3 1)

> (shuffle (iota 5))
(0 1 4 2 3)
```
