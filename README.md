
# poLCAExtra : New and convenient functions to improve workflow of `poLCA`

The library `poLCAExtra` offers convenient functions to improve the
workflow of [`poLCA`](https://github.com/dlinzer/poLCA)(Linzer & Lewis,
2011)

``` r
# Current development version
remotes::install_github(repo = "quantmeth/poLCA")
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

    ## [1] 0.8396914

Predicted probabilities and classes

``` r
head(round(predict(LCA3), 3))
```

    ##   Pr(Class==1) Pr(Class==2) Pr(Class==3) Pred
    ## 1        0.957        0.039        0.004    1
    ## 2        0.977        0.020        0.003    1
    ## 3        0.994        0.000        0.005    1
    ## 4        0.977        0.020        0.003    1
    ## 5        0.543        0.455        0.001    1
    ## 6        0.362        0.638        0.001    2

``` r
# head(round(predict(LCAE, nclass = 3),3))
```

Tech10 (in reference to Mplus) to investigate local independence.

``` r
poLCA.tech10(LCA3)
```

    ## 
    ## The 20 most frequent patterns
    ## 
    ##    pattern observed expected      z   chi llik.contribution     p check
    ## 1   111111      200  201.256 -0.089 0.008            -2.504 0.465      
    ## 23  122121      166  165.339  0.051 0.003             1.325 0.480      
    ## 10  112121       48   47.670  0.048 0.002             0.662 0.481      
    ## 17  121121       42   46.128 -0.608 0.369            -7.875 0.272      
    ## 22  122111       39   37.760  0.202 0.041             2.520 0.420      
    ## 3   111121       38   32.910  0.887 0.787            10.930 0.187      
    ## 5   111211       32   30.693  0.236 0.056             2.669 0.407      
    ## 50  222222       29   28.136  0.163 0.027             1.754 0.435      
    ## 2   111112       25   27.201 -0.422 0.178            -4.219 0.337      
    ## 8   112111       23   28.326 -1.001 1.001            -9.581 0.158      
    ## 15  121111       22   18.606  0.787 0.619             7.373 0.216      
    ## 26  122222       11   12.684 -0.473 0.224            -3.134 0.318      
    ## 24  122122       10   10.900 -0.273 0.074            -1.724 0.393      
    ## 27  211111       10    7.975  0.717 0.514             4.525 0.237      
    ## 46  222122        9    8.921  0.026 0.001             0.159 0.489      
    ## 25  122221        8    5.429  1.103 1.218             6.203 0.135      
    ## 9   112112        7    2.989  2.320 5.382            11.914 0.010     *
    ## 42  221222        7    6.787  0.082 0.007             0.433 0.467      
    ## 6   111212        6    4.251  0.848 0.720             4.135 0.198      
    ## 14  112222        6    2.553  2.157 4.654            10.254 0.015     *
    ## 
    ## Number of empty cells:  14

``` r
# poLCA.residual.pattern(LCAE, nclass = 3)
```

Residual covariances

``` r
poLCA.residual.cov(LCAE, nclass = 3)
```

    ## 
    ##  Residual covariance matrix of Class == 1 
    ##  
    ##       V1    V2    V3    V4    V5    V6
    ## V1 0.067 0.033 0.026 0.056 0.027 0.060
    ## V2 0.033 0.244 0.132 0.011 0.138 0.019
    ## V3 0.026 0.132 0.161 0.014 0.142 0.011
    ## V4 0.056 0.011 0.014 0.026 0.026 0.057
    ## V5 0.027 0.138 0.142 0.026 0.143 0.034
    ## V6 0.060 0.019 0.011 0.057 0.034 0.041
    ## 
    ##  Residual covariance matrix of Class == 2 
    ##  
    ##        V1    V2    V3    V4    V5     V6
    ## V1 -0.100 0.017 0.045 0.096 0.054  0.031
    ## V2  0.017 0.115 0.140 0.027 0.133  0.021
    ## V3  0.045 0.140 0.102 0.000 0.128  0.036
    ## V4  0.096 0.027 0.000 0.012 0.010  0.092
    ## V5  0.054 0.133 0.128 0.010 0.160  0.024
    ## V6  0.031 0.021 0.036 0.092 0.024 -0.006
    ## 
    ##  Residual covariance matrix of Class == 3 
    ##  
    ##       V1    V2    V3    V4    V5    V6
    ## V1 0.083 0.029 0.021 0.054 0.028 0.059
    ## V2 0.029 0.121 0.162 0.012 0.165 0.024
    ## V3 0.021 0.162 0.086 0.018 0.104 0.025
    ## V4 0.054 0.012 0.018 0.134 0.014 0.064
    ## V5 0.028 0.165 0.104 0.014 0.100 0.015
    ## V6 0.059 0.024 0.025 0.064 0.015 0.096

``` r
# poLCA.residual.cov(LCA3)
```

Bootstrap 3-step approach

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
