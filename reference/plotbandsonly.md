# Adds uncertainty bounds to an existing plot.

Adds a polygon representing uncertainty bounds to an existing plot.

## Usage

``` r
plotbandsonly(lband, uband, dates, date.fmt="%Y-%m-%d",
          bands.col="lightblue", border= NA, ...)
```

## Arguments

- lband:

  zoo or xts object with the values of the lower band.

- uband:

  zoo or xts object with the values of the upper band.

- dates:

  OPTIONAL. Date, factor, or character object indicating the dates that
  will be assigned to `lband` and `uband`.  
  If `dates` is a factor or character vector, its values are converted
  to dates using the date format specified by `date.fmt`.  
  When `lband` and `uband` are already of zoo class, **the values
  provided by `dates` over-write the original dates of the objects**.

- date.fmt:

  OPTIONAL. Character indicating the format of `dates`. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).

- bands.col:

  See [`polygon`](https://rdrr.io/r/graphics/polygon.html). Color to be
  used for filling the area between the lower and upper uncertainty
  bound.

- border:

  See [`polygon`](https://rdrr.io/r/graphics/polygon.html). The color to
  draw the border. The default, 'NULL', means to use 'par("fg")'. Use
  'border = NA' to omit borders.

- ...:

  further arguments passed to the
  [`polygon`](https://rdrr.io/r/graphics/polygon.html) function for
  plotting the bands, or from other methods

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

## Note

It requires the hydroTSM package

## See also

[`pfactor`](https://hzambran.github.io/hydroGOF/reference/pfactor.md),
[`rfactor`](https://hzambran.github.io/hydroGOF/reference/rfactor.md)

## Examples

``` r
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Selecting only the daily values belonging to the year 1961
obs <- window(obs, end=as.Date("1961-12-31"))

# Generating the lower and upper uncertainty bounds
lband <- obs - 5
uband <- obs + 5

if (FALSE) { # \dontrun{
plot(obs, type="n")
plotbandsonly(lband, uband)
points(obs, col="blue", cex=0.6, type="o")
} # }
```
