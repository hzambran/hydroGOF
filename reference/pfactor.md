# P-factor

`P-factor` is the percent of observations that are within the given
uncertainty bounds.  

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
pfactor(x, ...)

# Default S3 method
pfactor(x, lband, uband, na.rm=TRUE, ...)

# S3 method for class 'data.frame'
pfactor(x, lband, uband, na.rm=TRUE, ...)

# S3 method for class 'matrix'
pfactor(x, lband, uband, na.rm=TRUE, ...)
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

  a logical value indicating whether 'NA' values should be stripped
  before the computation proceeds.

- ...:

  further arguments passed to or from other methods.

## Details

The `P-factor` quantifies the percentage of observed values that fall
within the prediction uncertainty band defined by the lower and upper
bounds. It is a measure of the coverage of the uncertainty interval.

Mathematically, the P-factor is defined as:

\$\$ P\text{-factor} = \frac{1}{N} \sum\_{i=1}^{N} I \left( lband_i \le
x_i \le uband_i \right) \$\$

where \\N\\ is the total number of observations, \\x_i\\ is the observed
value at time step \\i\\, and \\lband_i\\ and \\uband_i\\ are the lower
and upper uncertainty bounds, respectively. The function \\I(\cdot)\\ is
an indicator function that takes the value 1 when the observed value
lies within the uncertainty bounds and 0 otherwise.

The `P-factor` ranges from 0 to 1. A value of 1 indicates that all
observations are bracketed by the uncertainty bounds, whereas a value of
0 indicates that none of the observations fall within the bounds.

## Value

Percent of the `x` observations that are within the given uncertainty
bounds given by `lband` and `uband`.  

If `x`, `lband`, and `uband` are matrices, the returned value is a
vector with the `P-factor` computed for each column.

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

[`rfactor`](https://hzambran.github.io/hydroGOF/reference/rfactor.md),
[`plotbands`](https://hzambran.github.io/hydroGOF/reference/plotbands.md)

## Examples

``` r
x <- 1:10
lband <- x - 0.1
uband <- x + 0.1
pfactor(x, lband, uband)
#> [1] 1

lband <- x - rnorm(10)
uband <- x + rnorm(10)
pfactor(x, lband, uband)
#> [1] 0.4

#############
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Selecting only the daily values belonging to the year 1961
obs <- window(obs, end=as.Date("1961-12-31"))

# Generating the lower and upper uncertainty bounds, centred at the observations
lband <- obs - 5
uband <- obs + 5

pfactor(obs, lband, uband)
#> [1] 1

# Randomly generating the lower and upper uncertainty bounds
uband <- obs + rnorm(length(obs))
lband <- obs - rnorm(length(obs))

pfactor(obs, lband, uband)
#> [1] 0.2547945
```
