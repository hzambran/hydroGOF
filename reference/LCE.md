# Lee and Choi Efficiency

Lee and Choi Efficiency between `sim` and `obs`, with treatment of
missing values.

This goodness-of-fit measure was proposed by Lee and Choi (2022) as an
alternative to the Liu-Mean Efficiency (LME), designed to provide a
multi-dimensional and diagnostically balanced evaluation of model
performance; by jointly considering correlation, variability and bias;
whereas LME is fundamentally a single-error-based metric.

Unlike some single-error-based criteria, LCE explicitly combines the
correlation coefficient and the variability ratio through two
complementary terms, `r*Alpha` and `r/Alpha`, in order to penalize
inconsistent representations of timing and variability.

For a short description of its components and the numeric range of
values, please see Details.

## Usage

``` r
LCE(sim, obs, ...)

# Default S3 method
LCE(sim, obs, na.rm=TRUE, out.type=c("single", "full"),
             fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

# S3 method for class 'data.frame'
LCE(sim, obs, na.rm=TRUE, out.type=c("single", "full"),
             fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

# S3 method for class 'matrix'
LCE(sim, obs, na.rm=TRUE, out.type=c("single", "full"),
             fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

# S3 method for class 'zoo'
LCE(sim, obs, na.rm=TRUE, out.type=c("single", "full"),
             fun=NULL, ..., 
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

- out.type:

  character, indicating whether the output of the function has to
  include each one of the terms used in the computation of the Lee and
  Choi Efficiency or not. Valid values are:

  -) single: the output is a numeric with the Lee and Choi Efficiency
  only.

  -) full: the output is a list of two elements: the first one with the
  Lee and Choi Efficiency, and the second is a numeric with 5 elements:
  the Pearson product-moment correlation coefficient (‘r’), the
  variability ratio (‘Alpha’), the bias ratio (‘Beta’), the product
  between correlation and variability (‘rAlpha’), and the ratio between
  correlation and variability (‘rOverAlpha’).

- fun:

  function to be applied to `sim` and `obs` in order to obtain
  transformed values thereof before computing the Lee and Choi
  Efficiency.

  The first argument MUST BE a numeric vector with any name (e.g., `x`),
  and additional arguments are passed using `...`.

- ...:

  arguments passed to `fun`, in addition to the mandatory first numeric
  vector.

- epsilon.type:

  argument used to define a numeric value to be added to both `sim` and
  `obs` before applying `fun`.

  It was designed to allow the use of logarithm and other similar
  functions that do not work with zero values.

  Valid values of `epsilon.type` are:

  1\) "none": `sim` and `obs` are used by `fun` without the addition of
  any numeric value.

  2\) "Pushpalatha2012": one hundredth (1/100) of the mean observed
  values is added to both `sim` and `obs` before applying `fun`, as
  described in Pushpalatha et al. (2012).

  3\) "otherFactor": the numeric value defined in the `epsilon.value`
  argument is used to multiply the mean observed values, instead of the
  one hundredth (1/100) described in Pushpalatha et al. (2012). The
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

In the computation of this index, there are three main diagnostic
components involved:

1\) r : the Pearson product-moment correlation coefficient. Ideal value
is r=1.

2\) Alpha : the variability ratio, defined as the ratio between the
standard deviation of the simulated values and the standard deviation of
the observed ones. Ideal value is Alpha=1.

3\) Beta : the bias ratio, defined as the ratio between the mean of the
simulated values and the mean of the observed ones. Ideal value is
Beta=1.

The Lee and Choi Efficiency combines these terms through two additional
correlation-variability components:

\$\$r\alpha\$\$

and

\$\$r/\alpha\$\$

Both terms have an ideal value of 1. The first term, \\r\alpha\\,
penalizes cases where the combined effect of correlation and variability
is inconsistent with the observed dynamics. The second term,
\\r/\alpha\\, penalizes the opposite imbalance between correlation and
variability. Together, these two terms make LCE sensitive to
compensating errors between timing and dispersion.

Lee and Choi efficiencies range from -Inf to 1. Essentially, the closer
to 1, the more similar `sim` and `obs` are in terms of timing,
variability and bias.

\$\$LCE = 1 - ED\$\$

\$\$ ED = \sqrt{ (r\alpha - 1)^2 + (r/\alpha - 1)^2 + (\beta - 1)^2 }
\$\$

where:

\$\$r=Pearson product-moment correlation coefficient\$\$

\$\$\alpha=\sigma_s/\sigma_o\$\$

\$\$\beta=\mu_s/\mu_o\$\$

\$\$r\alpha=r\times\alpha\$\$

\$\$r/\alpha=r/\alpha\$\$

where \\\mu_s\\ and \\\mu_o\\ are the mean simulated and observed
values, respectively, and \\\sigma_s\\ and \\\sigma_o\\ are their
corresponding standard deviations.

A value of LCE=1 indicates perfect agreement between simulated and
observed values. Values close to 1 indicate strong agreement across
correlation, variability and bias, whereas negative values indicate
progressively poorer model performance.

## Value

If `out.type=single`: numeric with the Lee and Choi Efficiency between
`sim` and `obs`. If `sim` and `obs` are matrices, the output value is a
vector, with the Lee and Choi Efficiency between each column of `sim`
and `obs`.

If `out.type=full`: a list of two elements:

- LCE.value:

  numeric with the Lee and Choi Efficiency. If `sim` and `obs` are
  matrices, the output value is a vector, with the Lee and Choi
  Efficiency between each column of `sim` and `obs`

- LCE.elements:

  numeric with 5 elements: the Pearson product-moment correlation
  coefficient (‘r’), the variability ratio (‘Alpha’), the bias ratio
  (‘Beta’), the product between correlation and variability (‘rAlpha’),
  and the ratio between correlation and variability (‘rOverAlpha’). If
  `sim` and `obs` are matrices, the output value is a matrix, with the
  previous five elements computed for each column of `sim` and `obs`  

## References

Lee, J. S., & Choi, H. I. (2022). A rebalanced performance criterion for
hydrological model calibration. Journal of Hydrology, 606, 127372.
https://doi.org/10.1016/j.jhydrol.2021.127372

Liu, D. (2020). A rational performance criterion for hydrological model.
Journal of Hydrology, 590, 125488.
https://doi.org/10.1016/j.jhydrol.2020.125488

Choi, H. I. (2021). Comment on Liu (2020): A rational performance
criterion for hydrological model. Journal of Hydrology, 606, 126927.
https://doi.org/10.1016/j.jhydrol.2021.126927

Liu, D. (2021). Reply to "Comment on Liu (2020): A rational performance
criterion for a hydrological model" by HyunIl Choi. Journal of
Hydrology, 603, 126935. https://doi.org/10.1016/j.jhydrol.2021.126927

Gupta, H.V.; Kling, H.; Yilmaz, K.K.; Martinez, G.F. (2009).
Decomposition of the mean squared error and NSE performance criteria:
Implications for improving hydrological modelling. Journal of Hydrology,
377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003.

Pushpalatha, R.; Perrin, C.; Le Moine, N.; Andreassian, V. (2012). A
review of efficiency criteria suitable for evaluating low-flow
simulations. Journal of Hydrology, 420-421, 171-182.
doi:10.1016/j.jhydrol.2011.11.055.

## Author

Mauricio Zambrano-Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` have to have the same length/dimension  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation.

## See also

[`LME`](https://hzambran.github.io/hydroGOF/reference/LME.md),
[`KGE`](https://hzambran.github.io/hydroGOF/reference/KGE.md),
[`NSE`](https://hzambran.github.io/hydroGOF/reference/NSE.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
# Example1: basic ideal case
obs <- 1:10
sim <- 1:10
LCE(sim, obs)
#> [1] 1

obs <- 1:10
sim <- 2:11
LCE(sim, obs)
#> [1] 0.8181818

##################
# Example2: simulated values equal to observations plus random noise

set.seed(123)
obs <- 1:100
sim <- obs + rnorm(100, mean=0, sd=5)

LCE(sim=sim, obs=obs)
#> [1] 0.9612654

LCE(sim=sim, obs=obs, out.type="full")
#> $LCE.value
#> [1] 0.9612654
#> 
#> $LCE.elements
#>          r      Alpha       Beta     rAlpha rOverAlpha 
#>  0.9882186  1.0246269  1.0089511  1.0125554  0.9644668 
#> 

##################
# Example3: applying logarithmic transformation

LCE(sim=sim, obs=obs, fun=log)
#> Warning: NaNs produced
#> [1] 0.9212541

##################
# Example4: applying logarithmic transformation with Pushpalatha constant

LCE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")
#> Warning: NaNs produced
#> [1] 0.9262892
```
