# Modified Index of Agreement

This function computes the modified Index of Agreement between `sim` and
`obs`, with treatment of missing values.  

## Usage

``` r
md(sim, obs, ...)

# Default S3 method
md(sim, obs, j=1, na.rm=TRUE, fun=NULL, ...,
            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
            epsilon.value=NA)

# S3 method for class 'data.frame'
md(sim, obs, j=1, na.rm=TRUE, fun=NULL, ...,
            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
            epsilon.value=NA)

# S3 method for class 'matrix'
md(sim, obs, j=1, na.rm=TRUE, fun=NULL, ...,
            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
            epsilon.value=NA)

# S3 method for class 'zoo'
md(sim, obs, j=1, na.rm=TRUE, fun=NULL, ...,
            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
            epsilon.value=NA)
```

## Arguments

- sim:

  numeric, zoo, matrix or data.frame with simulated values

- obs:

  numeric, zoo, matrix or data.frame with observed values

- j:

  numeric, with the exponent to be used in the computation of the
  modified index of agreement. The default value is j=1.

- na.rm:

  a logical value indicating whether 'NA' should be stripped before the
  computation proceeds.  
  When an 'NA' value is found at the i-th position in `obs` **OR**
  `sim`, the i-th value of `obs` **AND** `sim` are removed before the
  computation.

- fun:

  function to be applied to `sim` and `obs` in order to obtain
  transformed values thereof before computing the modified index of
  agreement.

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
  any nummeric value.

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

  numeric value to be added to both `sim` and `obs` when
  `epsilon.type="otherValue"`.

## Details

\$\$md = 1 - \frac{ \sum\_{i=1}^N {\left\| O_i - S_i \right\| ^j} } {
\sum\_{i=1}^N { \left\| S_i - \bar{O} \right\| + \left\| O_i - \bar{O}
\right\|^j } } \$\$

The Index of Agreement (d) developed by Willmott (1981) as a
standardized measure of the degree of model prediction error and varies
between 0 and 1.  
A value of 1 indicates a perfect match, and 0 indicates no agreement at
all (Willmott, 1981).

The index of agreement can detect additive and proportional differences
in the observed and simulated means and variances; however, it is overly
sensitive to extreme values due to the squared differences (Legates and
McCabe, 1999).

## Value

Modified index of agreement between `sim` and `obs`.

If `sim` and `obs` are matrixes, the returned value is a vector, with
the modified index of agreement between each column of `sim` and `obs`.

## References

Krause, P.; Boyle, D.P.; Base, F. (2005). Comparison of different
efficiency criteria for hydrological model assessment, Advances in
Geosciences, 5, 89-97. doi:10.5194/adgeo-5-89-2005.  

Willmott, C.J. (1981). On the validation of models. Physical Geography,
2, 184–194. doi:10.1080/02723646.1981.10642213.

Willmott, C.J. (1984). On the evaluation of model performance in
physical geography. Spatial Statistics and Models, G. L. Gaile and C. J.
Willmott, eds., 443-460. doi:10.1007/978-94-017-3048-8_23.

Willmott, C.J.; Ackleson, S.G. Davis, R.E.; Feddema, J.J.; Klink, K.M.;
Legates, D.R.; O'Donnell, J.; Rowe, C.M. (1985), Statistics for the
Evaluation and Comparison of Models, J. Geophys. Res., 90(C5),
8995-9005. doi:10.1029/JC090iC05p08995.

Legates, D.R.; McCabe, G. J. Jr. (1999), Evaluating the Use of
"Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model
Validation, Water Resour. Res., 35(1), 233-241.
doi:10.1029/1998WR900018.

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` has to have the same length/dimension  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation

## See also

[`d`](https://hzambran.github.io/hydroGOF/reference/d.md),
[`dr`](https://hzambran.github.io/hydroGOF/reference/dr.md),
[`rd`](https://hzambran.github.io/hydroGOF/reference/rd.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
obs <- 1:10
sim <- 1:10
md(sim, obs)
#> [1] 1

obs <- 1:10
sim <- 2:11
md(sim, obs)
#> [1] 0.8039216

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the modified index of agreement for the "best" (unattainable) case
md(sim=sim, obs=obs)
#> [1] 1

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'd1'
md(sim=sim, obs=obs)
#> [1] 0.7819402
```
