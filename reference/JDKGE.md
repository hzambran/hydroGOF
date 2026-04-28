# Joint Divergence Kling-Gupta Efficiency

Joint Divergence Kling-Gupta efficiency between `sim` and `obs`, with
treatment of missing values.

This goodness-of-fit measure extends the traditional Kling-Gupta
efficiency by incorporating a fourth diagnostic component that evaluates
the similarity between the probability distributions of simulated and
observed values. This additional component is based on a divergence
measure computed from the logarithm of flows, allowing the metric to
explicitly account for differences in distributional shape, including
the representation of low and high flows.

The resulting index provides a more comprehensive evaluation of
hydrological model performance by jointly considering correlation,
variability, bias, and distributional similarity.

For a short description of its four components and the numeric range of
values, please see Details.

## Usage

``` r
JDKGE(sim, obs, ...)

# Default S3 method
JDKGE(sim, obs,
s=c(1,1,1,1),
na.rm=TRUE,
out.type=c("single","full"),
fun=NULL, ...,
epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
epsilon.value=NA,
density.method=c("hist","kde"),
nbins="Sturges")

# S3 method for class 'data.frame'
JDKGE(sim, obs,
s=c(1,1,1,1),
na.rm=TRUE,
out.type=c("single","full"),
fun=NULL, ...,
epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
epsilon.value=NA,
density.method=c("hist","kde"),
nbins="Sturges")

# S3 method for class 'matrix'
JDKGE(sim, obs,
s=c(1,1,1,1),
na.rm=TRUE,
out.type=c("single","full"),
fun=NULL, ...,
epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
epsilon.value=NA,
density.method=c("hist","kde"),
nbins="Sturges")

# S3 method for class 'zoo'
JDKGE(sim, obs,
s=c(1,1,1,1),
na.rm=TRUE,
out.type=c("single","full"),
fun=NULL, ...,
epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
epsilon.value=NA,
density.method=c("hist","kde"),
nbins="Sturges")
```

## Arguments

- sim:

  numeric, zoo, matrix or data.frame with simulated values

- obs:

  numeric, zoo, matrix or data.frame with observed values

- s:

  numeric of length 4, representing the scaling factors to be used for
  re-scaling the criteria space before computing the Euclidean distance
  from the ideal point c(1,1,1,1), i.e., `s` elements are used for
  adjusting the emphasis on different components.

  The first element is used for rescaling the Pearson product-moment
  correlation coefficient (`r`), the second element is used for
  rescaling the variability ratio (`Alpha`), the third element is used
  for rescaling the bias ratio (`Beta`), and the fourth element is used
  for rescaling the distribution similarity component (`Delta`).

- na.rm:

  a logical value indicating whether 'NA' should be stripped before the
  computation proceeds.  

  When an 'NA' value is found at the i-th position in `obs` **OR**
  `sim`, the i-th value of `obs` **AND** `sim` are removed before the
  computation.

- out.type:

  character, indicating whether the output of the function has to
  include each one of the four terms used in the computation of the
  Joint Divergence Kling-Gupta efficiency or not. Valid values are:

  -) single: the output is a numeric with the Joint Divergence
  Kling-Gupta efficiency only.

  -) full: the output is a list of two elements: the first one with the
  Joint Divergence Kling-Gupta efficiency, and the second is a numeric
  with 4 elements: the Pearson product-moment correlation coefficient
  (‘r’), the variability ratio (‘Alpha’), the bias ratio (‘Beta’), and
  the distribution similarity component (‘Delta’).

- fun:

  function to be applied to `sim` and `obs` in order to obtain
  transformed values thereof before computing the Joint Divergence
  Kling-Gupta efficiency.

  The first argument MUST BE a numeric vector with any name (e.g., `x`),
  and additional arguments are passed using `...`.

- ...:

  arguments passed to `fun`, in addition to the mandatory first numeric
  vector.

  When `density.method="kde"`, additional arguments are also passed to
  the `density` function used for kernel density estimation (e.g.,
  bandwidth selection).

- epsilon.type:

  argument used to define a numeric value to be added to both `sim` and
  `obs` before applying `fun`.

  It was designed to allow the use of logarithm and other similar
  functions that do not work with zero values.

  Valid values of `epsilon.type` are:

  1\) "none": `sim` and `obs` are used by `fun` without the addition of
  any numeric value.

  2\) "Pushpalatha2012": one hundredth (1/100) of the mean observed
  values is added to both `sim` and `obs` before applying `fun`.

  3\) "otherFactor": the numeric value defined in the `epsilon.value`
  argument is used to multiply the mean observed values. The resulting
  value is then added to both `sim` and `obs`, before applying `fun`.

  4\) "otherValue": the numeric value defined in the `epsilon.value`
  argument is directly added to both `sim` and `obs`, before applying
  `fun`.

- epsilon.value:

  numeric value used when `epsilon.type="otherValue"` or
  `epsilon.type="otherFactor"`.

- density.method:

  character, indicating the method used to estimate the empirical
  probability density functions required to compute the divergence
  component. Valid values are:

  -) "hist": probability densities are estimated using histograms. This
  is the default option.

  -) "kde": probability densities are estimated using kernel density
  estimation.

- nbins:

  numeric or character, indicating the number of bins or the method used
  to determine the number of bins when `density.method="hist"`. Default
  is "Sturges".

## Details

In the computation of this index, there are four main components
involved:

1\) r : the Pearson product-moment correlation coefficient. Ideal value
is r=1.

2\) Alpha : the variability ratio, defined as the ratio between the
standard deviation of the simulated values and the standard deviation of
the observed ones. Ideal value is Alpha=1.

3\) Beta : the bias ratio, defined as the ratio between the mean of the
simulated values and the mean of the observed ones. Ideal value is
Beta=1.

4\) Delta : the distribution divergence component, derived from the
Jensen-Shannon divergence (JSD) computed from the logarithm of simulated
and observed values. Ideal value is Delta=1.

Joint Divergence Kling-Gupta efficiencies range from -Inf to 1.
Essentially, the closer to 1, the more similar `sim` and `obs` are
across correlation, variability, bias, and distributional
characteristics. Values close to zero indicate substantial deviations
from the ideal agreement, while increasingly negative values reflect
progressively poorer model performance relative to the observed
dynamics.

\$\$JDKGE = 1 - ED\$\$

\$\$ ED = \sqrt{ (s\[1\]\*(r-1))^2 + (s\[2\]\*(\alpha-1))^2 +
(s\[3\]\*(\beta-1))^2 + (s\[4\]\*(\Delta-1))^2 } \$\$

\$\$r=Pearson product-moment correlation coefficient\$\$

\$\$\alpha=\sigma_s/\sigma_o\$\$

\$\$\beta=\mu_s/\mu_o\$\$

\$\$\Delta=1 - JSD\$\$

where:

JSD = Jensen-Shannon divergence between the empirical probability
distributions of log-transformed simulated and observed values.

The distribution divergence component allows the metric to explicitly
evaluate differences in the overall distributional behavior of simulated
and observed series, including discrepancies in low-flow and high-flow
regimes that may not be fully captured by moment-based statistics alone.

Regarding computational aspects, the choice of `density.method`
influences both numerical cost and smoothness of the estimated
distributions. The option "hist" relies on histogram-based density
estimation and is computationally efficient, deterministic, and
generally suitable for large-scale calibration or Monte Carlo
simulations. The option "kde" relies on kernel density estimation, which
produces smoother probability density functions and may provide a more
stable estimate of distributional divergence, but typically requires
greater computational time and memory resources. Consequently, "hist" is
recommended for routine model evaluation and calibration workflows,
whereas "kde" may be preferable for diagnostic analyses or research
applications where detailed characterization of distributional
differences is required.

## Value

If `out.type=single`: numeric with the Joint Divergence Kling-Gupta
efficiency between `sim` and `obs`. If `sim` and `obs` are matrices, the
output value is a vector, with the efficiency computed between each
column of `sim` and `obs`.

If `out.type=full`: a list of two elements:

- JDKGE.value:

  numeric with the Joint Divergence Kling-Gupta efficiency.

- JDKGE.elements:

  numeric with 4 elements: the Pearson product-moment correlation
  coefficient (‘r’), the variability ratio (‘Alpha’), the bias ratio
  (‘Beta’), and the distribution similarity component (‘Delta’).

## References

Ficchi, A.; Bavera, D.; Grimaldi, S.; Moschini, F.; Pistocchi, A.;
Russo, C.; Salamon, P.; Toreti, A. (2026). Improving low and high flow
simulations at once: An enhanced metric for hydrological model
calibrations. EGUsphere \[preprint\],
https://doi.org/10.5194/egusphere-2026-43.

## Author

Mauricio Zambrano-Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` have to have the same length/dimension  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation

## See also

[`APFB`](https://hzambran.github.io/hydroGOF/reference/APFB.md),
[`NSE`](https://hzambran.github.io/hydroGOF/reference/NSE.md),
[`wNSE`](https://hzambran.github.io/hydroGOF/reference/wNSE.md),
[`wsNSE`](https://hzambran.github.io/hydroGOF/reference/wsNSE.md),
[`HFB`](https://hzambran.github.io/hydroGOF/reference/HFB.md),
[`PMR`](https://hzambran.github.io/hydroGOF/reference/PMR.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
# Example 1: ideal case
obs <- 1:10
sim <- 1:10

JDKGE(sim, obs)
#> [1] 1

##################
# Example 2: simulated values equal to twice the observations

sim <- 2*obs

JDKGE(sim=sim, obs=obs, out.type="full")
#> $JDKGE.value
#> [1] -0.02139159
#> 
#> $JDKGE.elements
#>         r      Beta     Gamma     Delta 
#> 1.0000000 2.0000000 1.0000000 0.7920558 
#> 

##################
# Example 3: using logarithmic transformation

JDKGE(sim=sim, obs=obs, fun=log)
#> Warning: Non-positive values detected => log-transform not possible
#> [1] NA

##################
# Example 4: using kernel density estimation

JDKGE(sim=sim, obs=obs, density.method="kde")
#> [1] -0.002352203
```
