# Weighted seasonal Nash-Sutcliffe Efficiency

Weighted seasonal Nash-Sutcliffe Efficiency between `sim` and `obs`,
with treatment of missing values.

This function is designed to identify differences in high or low values,
depending on the user-defined value given to the `lambda` argument. See
Usage and Details.

## Usage

``` r
wsNSE(sim, obs, ...)

# Default S3 method
wsNSE(sim, obs, na.rm=TRUE, 
              j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

# S3 method for class 'data.frame'
wsNSE(sim, obs, na.rm=TRUE, 
              j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

# S3 method for class 'matrix'
wsNSE(sim, obs, na.rm=TRUE, 
              j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)
             
# S3 method for class 'zoo'
wsNSE(sim, obs, na.rm=TRUE, 
              j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)
```

## Arguments

- sim:

  numeric, zoo, matrix or data.frame with simulated values

- obs:

  numeric, zoo, matrix or data.frame with observed values

- na.rm:

  a logical value indicating whether 'NA' should be stripped before the
  computation proceeds.  
  When an 'NA' value is found at the i-th position in `obs` **OR**
  `sim`, the i-th value of `obs` **AND** `sim` are removed before the
  computation.

- j:

  numeric, representing an arbitrary value used to power the differences
  between observations and simulations. By default `j=2`, which mimics
  the traditional Nash-Sutcliffe function, mainly focused on thr
  representation of high values. For low flows, suggested values for `j`
  are 1, 1/2 or 1/3. See Legates and McCabe, (1999) and Krausse et
  al. (2005) for a discussion of suggested values of `j`.

- lambda:

  numeric in \[0, 1\] representing the weight given to the high observed
  values. The closer the `lambda=1` value is to 1, the higher the weight
  given to high values. On the contrary, the closer the `lambda=1` value
  is to 0, the higher the weight given to low values.

  Low values get a weight equal to `1-lambda`. Between high and low
  values there is a linear transition from `lambda` to `1-lambda`,
  respectively.

  Suggested values for lambda are `lambda=0.95` when focusing in high
  (streamflow) values and `lambda=0.05` when focusing in low
  (streamflow) values.

- lQ.thr:

  numeric, representing the non-exceedence probabiliy used to identify
  low flows in `obs`. All values in `obs` that are equal or lower than
  `quantile(obs, probs=(1-lQ.thr))` are considered as low values. By
  default `lQ.thr=0.6`.

  On the other hand, the low values in `sim` are those located at the
  same i-th position than the i-th value of the `obs` deemed as low
  flows.

- hQ.thr:

  numeric, representing the non-exceedence probabiliy used to identify
  high flows in `obs`. All values in `obs` that are equal or higher than
  `quantile(obs, probs=(1-hQ.thr))` are considered as high flows. By
  default `hQ.thr=0.1`.

  On the other hand, the high values in `sim` are those located at the
  same i-th position than the i-th value of the `obs` deemed as high
  flows.

- fun:

  function to be applied to `sim` and `obs` in order to obtain
  transformed values thereof before computing this goodness-of-fit
  index.

  The first argument MUST BE a numeric vector with any name (e.g., `x`),
  and additional arguments are passed using `...`.

- ...:

  arguments passed to `fun`, in addition to the mandatory first numeric
  vector.

- epsilon.type:

  argument used to define a numeric value to be added to both `sim` and
  `obs` before applying `fun`.

  It is was designed to allow the use of logarithm and other similar
  functions that do not work with zero values.

  Valid values of `epsilon.type` are:

  1\) "none": `sim` and `obs` are used by `fun` without the addition of
  any numeric value. This is the default option.

  2\) "Pushpalatha2012": one hundredth (1/100) of the mean observed
  values is added to both `sim` and `obs` before applying `fun`, as
  described in Pushpalatha et al. (2012).

  3\) "otherFactor": the numeric value defined in the `epsilon.value`
  argument is used to multiply the the mean observed values, instead of
  the one hundredth (1/100) described in Pushpalatha et al. (2012). The
  resulting value is then added to both `sim` and `obs`, before applying
  `fun`.

  4\) "otherValue": the numeric value defined in the `epsilon.value`
  argument is directly added to both `sim` and `obs`, before applying
  `fun`.

- epsilon.value:

  -) when `epsilon.type="otherValue"` it represents the numeric value to
  be added to both `sim` and `obs` before applying `fun`.  
  -) when `epsilon.type="otherFactor"` it represents the numeric factor
  used to multiply the mean of the observed values, instead of the one
  hundredth (1/100) described in Pushpalatha et al. (2012). The
  resulting value is then added to both `sim` and `obs` before applying
  `fun`.

## Details

The weighted seasonal Nash-Sutcliffe Efficiency was proposed by
Zambrano-Bigiarini and Bellin (2012), inspired by the classical
Nash-Sutcliffe efficiency (NSE, Nash and Sutcliffe, 1970), but designed
to give more emphasis to either high or low observed values.

In the implemented formulation, the low- and high-flow thresholds are
obtained from the observed series as:

\$\$lQ = Q\_{obs}(1-lQ.thr)\$\$ \$\$hQ = Q\_{obs}(1-hQ.thr)\$\$

where \\Q\_{obs}(p)\\ is the empirical quantile of `obs` at probability
\\p\\. A weight \\w_i\\ is then assigned to each observed value
\\obs_i\\ according to the following piecewise-linear function:

\$\$ w_i = \left\\ \begin{array}{ll} \lambda, & obs_i \ge hQ \cr
1-\lambda, & obs_i \le lQ \cr (1-\lambda) + (2\lambda - 1)\frac{obs_i -
lQ}{hQ - lQ}, & lQ \< obs_i \< hQ \end{array} \right. \$\$

Hence, `lambda` controls the emphasis of the metric:

- when `lambda > 0.5`, high observed values receive larger weights than
  low values;

- when `lambda < 0.5`, low observed values receive larger weights than
  high values;

- when `lambda = 0.5`, all values receive the same weight and the
  weighting becomes uniform.

Using these weights, `wsNSE` is computed as:

\$\$ wsNSE = 1 - \frac{\sum\_{i=1}^{n} \left\| w_i (obs_i - sim_i)
\right\|^j} {\sum\_{i=1}^{n} \left\| w_i (obs_i - \overline{obs})
\right\|^j} \$\$

where \\\overline{obs}\\ is the arithmetic mean of the observed series
after removing missing pairs, and \\j\\ is the user-defined exponent.
Therefore, the numerator is a weighted error term and the denominator is
the corresponding weighted dispersion of `obs` around its mean. This is
the exact mathematical formulation implemented in `wsNSE.R`.

Following the traditional NSE, `wsNSE` ranges from \\-\infty\\ to 1,
with an optimal value of 1. Larger values indicate smaller weighted
discrepancies between `sim` and `obs`.

## Value

numeric with the the weighted seasonal Nash-Sutcliffe Efficiency (wsNSE)
between `sim` and `obs`. If `sim` and `obs` are matrices, the output
value is a vector, with the the weighted seasonal Nash-Sutcliffe
Efficiency (wsNSE) between each column of `sim` and `obs`.

## References

Zambrano-Bigiarini, M.; Bellin, A. (2012). Comparing goodness-of-fit
measures for calibration of models focused on extreme events. EGU
General Assembly 2012, Vienna, Austria, 22-27 Apr 2012, EGU2012-11549-1.

Nash, J.E.; J.V. Sutcliffe. (1970). River flow forecasting through
conceptual models. Part 1: a discussion of principles, Journal of
Hydrology 10, pp. 282-290. doi:10.1016/0022-1694(70)90255-6.

Schaefli, B.; Gupta, H. (2007). Do Nash values have value?. Hydrological
Processes 21, 2075-2080. doi:10.1002/hyp.6825.

Criss, R. E.; Winston, W. E. (2008), Do Nash values have value?.
Discussion and alternate proposals. Hydrological Processes, 22:
2723-2725. doi:10.1002/hyp.7072.

Yilmaz, K. K.; Gupta, H. V.; Wagener, T. (2008), A process-based
diagnostic approach to model evaluation: Application to the NWS
distributed hydrologic model, Water Resources Research, 44, W09417,
doi:10.1029/2007WR006716.

Krause, P.; Boyle, D.P.; Base, F. (2005). Comparison of different
efficiency criteria for hydrological model assessment, Advances in
Geosciences, 5, 89-97. doi:10.5194/adgeo-5-89-2005.  

Legates, D.R.; McCabe, G. J. Jr. (1999), Evaluating the Use of
"Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model
Validation, Water Resour. Res., 35(1), 233-241.
doi:10.1029/1998WR900018.

## Author

Mauricio Zambrano-Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` has to have the same length/dimension  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation

## See also

[`NSE`](https://hzambran.github.io/hydroGOF/reference/NSE.md),
[`wNSE`](https://hzambran.github.io/hydroGOF/reference/wNSE.md),
`wsNSE`,
[`APFB`](https://hzambran.github.io/hydroGOF/reference/APFB.md),
[`KGElf`](https://hzambran.github.io/hydroGOF/reference/KGElf.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
##################
# Example 1: Looking at the difference between 'KGE', 'NSE', 'wNSE', 'wsNSE',
# 'APFB' and 'KGElf' for detecting differences in high flows

# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, created equal to the observed values and then 
# random noise is added only to high flows, i.e., those equal or higher than 
# the quantile 0.9 of the observed values.
sim      <- obs
hQ.thr   <- quantile(obs, probs=0.9, na.rm=TRUE)
hQ.index <- which(obs >= hQ.thr)
hQ.n     <- length(hQ.index)
sim[hQ.index] <- sim[hQ.index] + rnorm(hQ.n, mean=mean(sim[hQ.index], na.rm=TRUE))

# Traditional Kling-Gupta eficiency (Gupta and Kling, 2009)
KGE(sim=sim, obs=obs)
#> [1] 0.06805044

# Traditional Nash-Sutcliffe eficiency (Nash and Sutcliffe, 1970)
NSE(sim=sim, obs=obs)
#> [1] 0.004535027

# Weighted Nash-Sutcliffe efficiency (Hundecha and Bardossy, 2004)
wNSE(sim=sim, obs=obs)
#> [1] 0.2812819

# wsNSE (Zambrano-Bigiarini and Bellin, 2012):
wsNSE(sim=sim, obs=obs)
#> [1] -0.2216416

# APFB (Mizukami et al., 2019):
APFB(sim=sim, obs=obs)
#> [1] 0.2918619


##################
# Example 2: Looking at the difference between 'KGE', 'NSE', 'wsNSE',
# 'dr', 'rd', 'md', 'APFB' and 'KGElf' for detecting differences in low flows

# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, created equal to the observed values and then 
# random noise is added only to low flows, i.e., those equal or lower than 
# the quantile 0.4 of the observed values.
sim      <- obs
lQ.thr   <- quantile(obs, probs=0.4, na.rm=TRUE)
lQ.index <- which(obs <= lQ.thr)
lQ.n     <- length(lQ.index)
sim[lQ.index] <- sim[lQ.index] + rnorm(lQ.n, mean=mean(sim[lQ.index], na.rm=TRUE))

# Traditional Kling-Gupta eficiency (Gupta and Kling, 2009)
KGE(sim=sim, obs=obs)
#> [1] 0.8930697

# Traditional Nash-Sutcliffe eficiency (Nash and Sutcliffe, 1970)
NSE(sim=sim, obs=obs)
#> [1] 0.9840572

# Weighted seasonal Nash-Sutcliffe efficiency (Zambrano-Bigiarini and Bellin, 2012):
wsNSE(sim=sim, obs=obs, lambda=0.05, j=1/2)
#> [1] 0.6795807

# Refined Index of Agreement (Willmott et al., 2012):
dr(sim=sim, obs=obs)
#> [1] 0.9387014

# Relative Index of Agreement (Krause et al., 2005):
rd(sim=sim, obs=obs)
#> [1] 0.9075633

# Modified Index of Agreement (Krause et al., 2005):
md(sim=sim, obs=obs)
#> [1] 0.9346985

# KGElf (Garcia et al., 2017):
KGElf(sim=sim, obs=obs)
#> [1] 0.5857591


##################
# Example 3: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'wsNSE' for the "best" (unattainable) case
wsNSE(sim=sim, obs=obs)
#> [1] 1


##################
# Example 4: wsNSE for simulated values created equal to the observed values and then 
#            random noise is added only to high flows, i.e., those equal or higher than 
#            the quantile 0.9 of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

wsNSE(sim=sim, obs=obs, fun=log)
#> [1] 1

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
wsNSE(sim=lsim, obs=lobs)
#> [1] 1


##################
# Example 5: wsNSE for simulated values created equal to the observed values and then 
#            random noise is added only to high flows, i.e., those equal or higher than 
#            the quantile 0.9 of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

wsNSE(sim=sim, obs=obs, fun=fun1)
#> [1] 1

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
wsNSE(sim=sim1, obs=obs1)
#> [1] 1
```
