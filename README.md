
# poLCAExtra : New and convenient functions to improve workflow of `poLCA`

The library `poLCAExtra` offers convenient functions to improve the
workflow of [`poLCA`](https://github.com/dlinzer/poLCA)(Linzer & Lewis,
2011)

``` r
# Current development version
remotes::install_github(repo = "quantmeth/poLCAExtra")
```

# Comparing many latent class models

Either by using many latent

``` r
# poLCAExtra has two data sets examples
jd <- ex1.poLCA

# Formula
f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)

# Four latent classes analysis of increasing classes
LCA1 <- poLCA(f1, data = jd, nclass = 1, verbose = FALSE) 
LCA2 <- poLCA(f1, data = jd, nclass = 2, verbose = FALSE)
LCA3 <- poLCA(f1, data = jd, nclass = 3, verbose = FALSE)
LCA4 <- poLCA(f1, data = jd, nclass = 4, maxiter = 1000, nrep = 10, verbose = FALSE)
```

They can be compared with.

``` r
anova(LCA1, LCA2, LCA3, LCA4)
```

    ##   nclass df     llike      AIC      BIC  Classes.size Entropy Relative.Entropy
    ## 1      1 57 -2687.896 5387.793 5415.900           800   3.360                 
    ## 2      2 50 -2463.806 4953.612 5014.512        98|702   3.082            0.897
    ## 3      3 43 -2195.821 4431.642 4525.334   101|345|354   2.746            0.840
    ## 4      4 36 -2190.270 4434.540 4561.025 49|73|324|354   2.736            0.871
    ##       LMR      p
    ## 1               
    ## 2 426.893 < .001
    ## 3 510.513 < .001
    ## 4  10.574  0.158

The function readily gathers all relevant statitics to choose the number
of classes.

This new version of `poLCA` can also handle many number of classes and
yield the same output.

``` r
LCAE <- poLCA(f1, data = jd, nclass = 1:4)
LCAE
```

Both previous table can be easily exported.

The original `plot()` (for `poLCA`) function has been updated to account
for the multiple LCA.

``` r
plot(LCAE, nclass = 3)
```

![](README_files/figure-gfm/plotLCA-1.png)<!-- -->

# New features

## New indices

The `SABIC`, `CAIC`, and `AIC3` statistics have been added to `poLCA`.

``` r
LCA3$sabic
```

    ## [1] 4461.823

``` r
LCA3$caic
```

    ## [1] 4525.359

``` r
LCA3$aic3
```

    ## [1] 4451.642

The Lo-Mendell-Rubin statistic has been added.

``` r
poLCA.lmr(LCA3)
```

    ## $vlmr
    ## [1] 535.9702
    ## 
    ## $lmr
    ## [1] 510.513
    ## 
    ## $df
    ## [1] 7
    ## 
    ## $lmr.p
    ## [1] 4.401763e-106

The relative entropy, which is more often requested than the entropy,
has been added.

``` r
poLCA.relentropy(LCA3)
```

    ## [1] 0.8396916

## A `predict()` function

Predicted probabilities and classes

``` r
head(round(predict(LCA3), 3))
```

    ##   Pr(Class==1) Pr(Class==2) Pr(Class==3) Pred
    ## 1        0.004        0.039        0.957    3
    ## 2        0.003        0.020        0.977    3
    ## 3        0.005        0.000        0.994    3
    ## 4        0.003        0.020        0.977    3
    ## 5        0.001        0.455        0.543    3
    ## 6        0.001        0.638        0.362    2

``` r
# head(round(predict(LCAE, nclass = 3),3))
```

## A function to verify LCA assumptions

Tech10 (in reference to Mplus) to investigate local independence.

``` r
poLCA.tech10(LCA3)
```

    ## The 20 most frequent patterns
    ## 
    ##    pattern observed expected     z  chi llik.contribution    p check
    ## 1   111111      200   201.26 -0.09 0.01             -2.50 0.46      
    ## 23  122121      166   165.34  0.05 0.00              1.32 0.48      
    ## 10  112121       48    47.67  0.05 0.00              0.66 0.48      
    ## 17  121121       42    46.13 -0.61 0.37             -7.88 0.27      
    ## 22  122111       39    37.76  0.20 0.04              2.52 0.42      
    ## 3   111121       38    32.91  0.89 0.79             10.93 0.19      
    ## 5   111211       32    30.69  0.24 0.06              2.67 0.41      
    ## 50  222222       29    28.14  0.16 0.03              1.75 0.44      
    ## 2   111112       25    27.20 -0.42 0.18             -4.22 0.34      
    ## 8   112111       23    28.33 -1.00 1.00             -9.58 0.16      
    ## 15  121111       22    18.61  0.79 0.62              7.37 0.22      
    ## 26  122222       11    12.68 -0.47 0.22             -3.13 0.32      
    ## 24  122122       10    10.90 -0.27 0.07             -1.72 0.39      
    ## 27  211111       10     7.97  0.72 0.51              4.53 0.24      
    ## 46  222122        9     8.92  0.03 0.00              0.16 0.49      
    ## 25  122221        8     5.43  1.10 1.22              6.20 0.13      
    ## 9   112112        7     2.99  2.32 5.38             11.91 0.01     *
    ## 42  221222        7     6.79  0.08 0.01              0.43 0.47      
    ## 6   111212        6     4.25  0.85 0.72              4.14 0.20      
    ## 14  112222        6     2.55  2.16 4.65             10.25 0.02     *
    ## 
    ## Number of observed patterns:  50
    ## Number of empty cells:  14
    ## Total number of possible patterns:  64

``` r
# poLCA.residual.pattern(LCAE, nclass = 3)
```

Residual covariances

``` r
# TODO
# poLCA.residual.cov(LCAE, nclass = 3)
# poLCA.residual.cov(LCA3)
```

## Bootstrap 3-step approach

``` r
# Tester des variables supplémentaires
d3 <- d3step("categorical", LCAE, nclass = 3)
# d3step("categorical", LCA3)

r3 <- r3step("continuous", LCAE, nclass = 3)
# r3step("continuous", LCA3)
```

The 3-step approches will be improved.

d3step plot

``` r
plot(d3, ci = c(.05,.95))
```

![](README_files/figure-gfm/d3stepplot-1.png)<!-- -->

r3step plot

``` r
plot(r3, ci = c(.05, .95))
```

![](README_files/figure-gfm/r3stepplot-1.png)<!-- -->

# How to cite

Caron, P.-O. (2024). *poLCAExtra : New and Convenient Functions for the
Package ‘poLCA’*. <https://github.com/quantmeth/poLCAExtra>

# References

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-poLCA" class="csl-entry">

Linzer, D. A., & Lewis, J. B. (2011). <span class="nocase">poLCA</span>:
An R package for polytomous variable latent class analysis. *Journal of
Statistical Software*, *42*(10), 1–29.
<https://www.jstatsoft.org/v42/i10/>

</div>

</div>
