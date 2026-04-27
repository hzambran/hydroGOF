# Valid Indexes

Identify the indexes that are simultaneously valid (not missing) in
`sim` and `obs`.

## Usage

``` r
valindex(sim, obs, ...)

# Default S3 method
valindex(sim, obs, ...)

# S3 method for class 'matrix'
valindex(sim, obs, ...)
```

## Arguments

- sim:

  zoo, xts, numeric, matrix or data.frame with simulated values

- obs:

  zoo, xts, numeric, matrix or data.frame with observed values

- ...:

  further arguments passed to or from other methods.

## Value

A vector with the indexes that are simultaneously valid (not missing) in
`obs` and `sim`.

## Author

Mauricio Zambrano Bigiarini \<mauricio.zambrano@ing.unitn.it\>

## Note

This function is used in the functions of this package for removing
missing values from the observed and simulated time series.

## See also

[`is.na`](https://rdrr.io/r/base/NA.html),
[`which`](https://rdrr.io/r/base/which.html)

## Examples

``` r
sim <- 1:5
obs <- c(1, NA, 3, NA, 5)
valindex(sim, obs)
#> [1] 1 3 5
```
