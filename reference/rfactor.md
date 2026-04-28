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
#> [1] -0.3211734

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
#> [1] 0.00350732
```
