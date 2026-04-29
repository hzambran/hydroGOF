# R-factor

`R-factor` represents the average width of the given uncertainty bounds
divided by the standard deviation of the observations.  

Ideally, i.e., with a combination of model structure and parameter
values that perfectly represents the catchment under study, and in
absence of measurement errors and other additional sources of
uncertainty, all the simulated values should be in a perfect match with
the observations, leading to a `P-factor` equal to 1, and an `R-factor`
equal to zero. However, in real-world applications we aim at
encompassing as much observations as possible within the given
uncertainty bounds (`P-factor` close to 1) while keeping the width of
the uncertainty bounds as small as possible (`R-factor` close to 0), in
order to avoid obtaining a good bracketing of observations at expense of
uncertainty bounds too wide to be informative for the decision-making
process.

## Usage

``` r
rfactor(x, ...)

# Default S3 method
rfactor(x, lband, uband, na.rm=TRUE, ...)

# S3 method for class 'data.frame'
rfactor(x, lband, uband, na.rm=TRUE, ...)

# S3 method for class 'matrix'
rfactor(x, lband, uband, na.rm=TRUE, ...)
```

## Arguments

- x:

  ts or zoo object with the observed values.

- lband:

  numeric, ts or zoo object with the values of the lower uncertainty
  bound

- uband:

  numeric, ts or zoo object with the values of the upper uncertainty
  bound

- na.rm:

  logical value indicating whether 'NA' values should be stripped before
  the computation proceeds.

- ...:

  further arguments passed to or from other methods.

## Details

The R-factor quantifies the average width of the prediction uncertainty
band relative to the variability of the observed data. It is a measure
of the magnitude of predictive uncertainty associated with a model
simulation.

Mathematically, the R-factor is defined as:

\$\$ R\text{-factor} = \frac{\overline{d_x}}{\sigma_x} \$\$

where \\\sigma_x\\ is the standard deviation of the observed variable
\\x\\, and \\\overline{d_x}\\ is the average thickness of the
uncertainty band, computed as:

\$\$ \overline{d_x} = \frac{1}{N} \sum\_{i=1}^{N} \left( uband_i -
lband_i \right) \$\$

where \\N\\ is the total number of observations, and \\lband_i\\ and
\\uband_i\\ are the lower and upper uncertainty bounds, respectively, at
time step \\i\\.

The `R-factor` ranges from 0 to infinity, with an optimal value of 0
indicating perfect agreement between simulated and observed values
(i.e., zero prediction uncertainty). In practical applications, the
`R-factor` represents the width of the uncertainty interval and should
be as small as possible. Values close to or smaller than 1 are commonly
considered indicative of an acceptable level of predictive uncertainty,
although acceptable thresholds depend on the quality of observations and
the modeling context.

Because a larger fraction of observations can often be bracketed by
widening the uncertainty bounds, the `R-factor` is typically interpreted
jointly with the `P-factor`. A balance between a high `P-factor` (good
coverage) and a low `R-factor` (narrow uncertainty bounds) is therefore
sought during model calibration and uncertainty analysis.

## Value

Average width of the given uncertainty bounds, given by `lband` and
`uband`, divided by the standard deviation of the observations `x`  

If `sim` and `obs` are matrixes, the returned value is a vector, with
the R-factor between each column of `sim` and `obs`.

## References

Abbaspour, K.C.; Faramarzi, M.; Ghasemi, S.S.; Yang, H. (2009),
Assessing the impact of climate change on water resources in Iran, Water
Resources Research, 45(10), W10,434, doi:10.1029/2008WR007615.

Abbaspour, K.C., Yang, J. ; Maximov, I.; Siber, R.; Bogner, K.;
Mieleitner, J. ; Zobrist, J.; Srinivasan, R. (2007), Modelling hydrology
and water quality in the pre-alpine/alpine Thur watershed using SWAT,
Journal of Hydrology, 333(2-4), 413-430,
doi:10.1016/j.jhydrol.2006.09.014.

Schuol, J.; Abbaspour, K.C.; Srinivasan, R.; Yang, H. (2008b),
Estimation of freshwater availability in the West African sub-continent
using the SWAT hydrologic model, Journal of Hydrology, 352(1-2), 30,
doi:10.1016/j.jhydrol.2007.12.025

Abbaspour, K.C. (2007), User manual for SWAT-CUP, SWAT calibration and
uncertainty analysis programs, 93pp, Eawag: Swiss Fed. Inst. of Aquat.
Sci. and Technol. Dubendorf, Switzerland.

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

## Note

So far, the argument `na.rm` is not being taken into account.

## See also

[`pfactor`](https://hzambran.github.io/hydroGOF/reference/pfactor.md),
[`plotbands`](https://hzambran.github.io/hydroGOF/reference/plotbands.md)

## Examples

``` r
x <- 1:10
lband <- x - 0.1
uband <- x + 0.1
rfactor(x, lband, uband)
#> [1] 0.06605783

lband <- x - rnorm(10)
uband <- x + rnorm(10)
rfactor(x, lband, uband)
#> [1] 0.1307914

#############
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Selecting only the daily values belonging to the year 1961
obs <- window(obs, end=as.Date("1961-12-31"))

# Generating the lower and upper uncertainty bounds, centred at the observations
lband <- obs - 5
uband <- obs + 5

rfactor(obs, lband, uband)
#> [1] 0.3312039

# Randomly generating the lower and upper uncertainty bounds
uband <- obs + rnorm(length(obs))
lband <- obs - rnorm(length(obs))

rfactor(obs, lband, uband)
#> [1] -0.0006681959
```
