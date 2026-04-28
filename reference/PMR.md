# Proxy for Model Robustness

Proxy for model robustness (PMR) between `sim` and `obs`, with treatment
of missing values.

## Usage

``` r
PMR(sim, obs, ...)

# Default S3 method
PMR(sim, obs, k=NULL, min.years=5, days.per.year=365,
             na.rm=TRUE, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

# S3 method for class 'data.frame'
PMR(sim, obs, k=NULL, min.years=5, days.per.year=365,
             na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

# S3 method for class 'matrix'
PMR(sim, obs, k=NULL, min.years=5, days.per.year=365,
             na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

# S3 method for class 'zoo'
PMR(sim, obs, k=NULL, min.years=5, days.per.year=365,
             na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)
```

## Arguments

- sim:

  numeric, zoo, matrix or data.frame with simulated values

- obs:

  numeric, zoo, matrix or data.frame with observed values

- k:

  integer value representing the length of the moving window (number of
  time steps) used to compute the bias over sub-periods.  

  The k argument should reflect the temporal scale at which robustness
  is intended to be evaluated, and therefore depends primarily on the
  time resolution of the data. Royer-Gaspard et al. (2021) recommended
  to use multi-year windows, typically in the range of 3 to 5 years, to
  ensure that each sub-period captures meaningful hydroclimatic
  variability while still allowing enough windows for comparison.

- min.years:

  only used when the user does not explicitly define the value of `k`,
  i.e., when `k=NULL`.  

  Minimum numbers of years used to ensure that each sub-period used int
  eh computation of PMR captures meaningful hydroclimatic variability
  while still allowing enough windows for comparison. By default,
  `min.years=5`.

- days.per.year:

  only used when the user does not explicitly define the value of `k`,
  i.e., when `k=NULL`.  

  Number of days in a year. A value of Use 365.25 is recoomended instead
  of the default value of 365 when `sim` and `obs` are long
  climatological series.

- na.rm:

  a logical value indicating whether 'NA' should be stripped before the
  computation proceeds.  
  When an 'NA' value is found at the i-th position in `obs` **OR**
  `sim`, the i-th value of `obs` **AND** `sim` are removed before the
  computation.

- fun:

  function to be applied to `sim` and `obs` in order to obtain
  transformed values thereof before computing the proxy for model
  robustness.

  The first argument MUST BE a numeric vector with any name (e.g., `x`),
  and additional arguments are passed using `...`.

- ...:

  arguments passed to `fun`, in addition to the mandatory first numeric
  vector.

- epsilon.type:

  argument used to define a numeric value to be added to both `sim` and
  `obs` before applying `FUN`.

  It was designed to allow the use of logarithm and other similar
  functions that do not work with zero values.

  Valid values of `epsilon.type` are:

  1\) "none": `sim` and `obs` are used by `fun` without the addition of
  any numeric value. This is the default option.

  2\) "Pushpalatha2012": one hundredth (1/100) of the mean observed
  values is added to both `sim` and `obs` before applying `fun`, as
  described in Pushpalatha et al. (2012).

  3\) "otherFactor": the numeric value defined in the `epsilon.value`
  argument is used to multiply the mean of the observed values. The
  resulting value is then added to both `sim` and `obs` before applying
  `fun`.

  4\) "otherValue": the numeric value defined in the `epsilon.value`
  argument is directly added to both `sim` and `obs` before applying
  `fun`.

- epsilon.value:

  -) when `epsilon.type="otherValue"` it represents the numeric value to
  be added to both `sim` and `obs` before applying `fun`.  
  -) when `epsilon.type="otherFactor"` it represents the numeric factor
  used to multiply the mean of the observed values to obtain the
  constant to be added to both `sim` and `obs` before applying `fun`.

## Details

\$\$ PMR = 2 \times \frac{1}{N} \sum\_{i=1}^{N} \left\| (\bar{S}\_i -
\bar{O}\_i) - (\bar{S} - \bar{O}) \right\| \frac{1}{\bar{O}} \$\$

where:

- \\\bar{S}\_i\\ and \\\bar{O}\_i\\ are the mean simulated and observed
  values computed over the i-th moving window of length `k`,

- \\\bar{S}\\ and \\\bar{O}\\ are the overall mean simulated and
  observed values,

- \\N\\ is the number of moving windows.

The proxy for model robustness (PMR) is a dimensionless statistic that
quantifies the temporal stability of model bias by measuring the average
deviation of sub-period bias from the overall bias.

PMR indicates how consistent the model performance is across different
time periods or hydrological conditions.

The proxy for model robustness ranges from 0 to positive infinity.
Essentially, the closer to 0, the more temporally robust the model is.  

-) PMR = 0 corresponds to a perfectly robust model, in which the model
bias is identical across all sub-periods.  

-) 0 \< PMR \< 1 indicates relatively stable model performance with
moderate temporal variability in bias.  

-) PMR \> 1 indicates increasing variability in model bias across time
periods, suggesting reduced robustness of model performance.

## Value

Proxy for model robustness between `sim` and `obs`.  

If `sim` and `obs` are matrixes, the returned value is a vector, with
the proxy for model robustness between each column of `sim` and `obs`.

## References

Royer-Gaspard, P., Thirel, G., Andreassian, V., Perrin, C., and Coron,
L. (2021). A robust and efficient framework for evaluating hydrological
model robustness. Hydrology and Earth System Sciences, 25, 5703–5726.
doi:10.5194/hess-25-5703-2021.

Pushpalatha, R.; Perrin, C.; Le Moine, N.; Andreassian, V. (2012). A
review of efficiency criteria suitable for evaluating low-flow
simulations. Journal of Hydrology, 420, 171–182.
doi:10.1016/j.jhydrol.2011.11.055.

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` have to have the same length/dimension.  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation.

The choice of window length `k` influences the temporal scale at which
robustness is evaluated.

## See also

[`pbias`](https://hzambran.github.io/hydroGOF/reference/pbias.md),
[`NSE`](https://hzambran.github.io/hydroGOF/reference/NSE.md),
[`VE`](https://hzambran.github.io/hydroGOF/reference/VE.md),
[`JDKGE`](https://hzambran.github.io/hydroGOF/reference/JDKGE.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
##################
# Example 1: ideal case
obs <- 1:100
sim <- 1:100

PMR(sim, obs, k=10)
#> [1] 0

##################
# Example 2:
# Simulated values with systematic bias in the second half

set.seed(123)

obs <- 1:100
sim <- obs

sim[51:100] <- sim[51:100] + 5

PMR(sim, obs, k=10)
#> [1] 0.0935698

##################
# Example 3:
# Applying logarithmic transformation

PMR(sim, obs, k=10, fun=log)
#> [1] 0.01744629

##################
# Example 4:
# Using Pushpalatha2012 epsilon

PMR(sim, obs, k=10, fun=log, epsilon.type="Pushpalatha2012")
#> [1] 0.01720871

##################
# Example 5:
# Matrix input (multi-site case)

obs <- cbind(site1=1:100, site2=1:100)

sim <- obs
sim[51:100,2] <- sim[51:100,2] + 10

PMR(sim, obs, k=10)
#>     site1     site2 
#> 0.0000000 0.1871396 
```
