# Plot a ts with observed values and two confidence bounds

It plots a ts with observed values and two confidence bounds. Optionally
can also add a simulated time series, in order to be compared with 'x'.

## Usage

``` r
plotbands(x, lband, uband, sim, 
          dates, date.fmt="%Y-%m-%d",
          gof.leg= TRUE, gof.digits=2, 
          legend=c("Obs", "Sim", "95PPU"), leg.cex=1,
          bands.col="lightblue", border= NA,
          tick.tstep= "auto", lab.tstep= "auto", lab.fmt=NULL,
          cal.ini=NA, val.ini=NA, 
          main="Confidence Bounds for 'x'", 
          xlab="Time", ylab="Q, [m3/s]", ylim,
          col=c("black", "blue"), type= c("lines", "lines"),
          cex= c(0.5, 0.5), cex.axis=1.2, cex.lab=1.2,          
          lwd=c(0.6, 1), lty=c(3, 4), pch=c(1,9), ...)
```

## Arguments

- x:

  zoo or xts object with the observed values.

- lband:

  zoo or xts object with the values of the lower band.

- uband:

  zoo or xts object with the values of the upper band.

- sim:

  OPTIONAL. zoo or xts object with the simulated values.

- dates:

  OPTIONAL. Date, factor, or character object indicating the dates that
  will be assigned to `x`, `lband`, `uband`, and `sim` (when
  provided).  
  If `dates` is a factor or character vector, its values are converted
  to dates using the date format specified by `date.fmt`.  
  When `x`, `lband`, `uband`, and `sim` are already of zoo class, **the
  values provided by `dates` over-write the original dates of the
  objects**.

- date.fmt:

  OPTIONAL. Character indicating the format in which the dates entered
  are stored in `cal.ini` and `val.ini`. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  Default value is %Y-%m-%d  
  ONLY required when `cal.ini`, `val.ini` or `dates` is provided.

- gof.leg:

  logical indicating if the p-factor and r-factor have to be computed
  and plotted as legends on the graph.

- gof.digits:

  OPTIONAL, numeric. Only used when `gof.leg=TRUE`. Decimal places used
  for rounding the goodness-of-fit indexes

- legend:

  OPTIONAL. logical or character vector of length 3 with the strings
  that will be used for the legend of the plot.  
  -) When `legend` is a character vector, the first element is used for
  labelling the observed series, the second for labelling the simulated
  series and the third one for the predictive uncertainty bounds.
  Default value is `c("obs", "sim", "95PPU")`  
  -) When `legend=FALSE`, the legend is not drawn.

- leg.cex:

  OPTIONAL. numeric. Used for the GoF legend. Character expansion factor
  \*relative\* to current 'par("cex")'. Used for text, and provides the
  default for 'pt.cex' and 'title.cex'. Default value is 1.

- bands.col:

  See [`polygon`](https://rdrr.io/r/graphics/polygon.html). Color to be
  used for filling the area between the lower and upper uncertainty
  bound.

- border:

  See [`polygon`](https://rdrr.io/r/graphics/polygon.html). The color to
  draw the border. The default, 'NULL', means to use 'par("fg")'. Use
  'border = NA' to omit borders.

- tick.tstep:

  character, indicating the time step that have to be used for putting
  the ticks on the time axis. Valid values are: auto, years,
  months,weeks, days, hours, minutes, seconds.

- lab.tstep:

  character, indicating the time step that have to be used for putting
  the labels on the time axis. Valid values are: auto, years,
  months,weeks, days, hours, minutes, seconds.

- lab.fmt:

  Character indicating the format to be used for the label of the axis.
  See `lab.fmt` in
  [`drawTimeAxis`](https://rdrr.io/pkg/hydroTSM/man/drawxaxis.html).

- cal.ini:

  OPTIONAL. Character with the date in which the calibration period
  started.  
  ONLY used for drawing a vertical red line at this date.

- val.ini:

  OPTIONAL. Character with the date in which the validation period
  started.  
  ONLY used for drawing a vertical red line at this date.

- main:

  an overall title for the plot: see 'title'

- xlab:

  a title for the x axis: see 'title'

- ylab:

  a title for the y axis: see 'title'

- ylim:

  the y limits of the plot. See
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html).

- col:

  colors to be used for plotting the `x` and `sim` ts.

- type:

  character. Indicates if the observed and simulated series have to be
  plotted as lines or points. Possible values are:  
  -) lines : the observed/simulated series are plotted as lines  
  -) points: the observed/simulated series are plotted as points

- cex:

  See code [plot.default](https://rdrr.io/r/graphics/plot.default.html).
  A numerical vector giving the amount by which plotting characters and
  symbols should be scaled relative to the default.  
  This works as a multiple of 'par("cex")'. 'NULL' and 'NA' are
  equivalent to '1.0'. Note that this does not affect annotation.

- cex.axis:

  magnification of axis annotation relative to 'cex'.

- cex.lab:

  Magnification to be used for x and y labels relative to the current
  setting of 'cex'. See '?par'.

- lwd:

  See [`plot.default`](https://rdrr.io/r/graphics/plot.default.html).
  The line width, see 'par'.

- lty:

  See [`plot.default`](https://rdrr.io/r/graphics/plot.default.html).
  The line type, see 'par'.

- pch:

  numeric, with the type of symbol for `x` and `y`. (e.g.: 1: white
  circle; 9: white rhombus with a cross inside)

- ...:

  further arguments passed to the
  [`points`](https://rdrr.io/r/graphics/points.html) function for
  plotting `x`, or from other methods

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
plotbands(obs, lband, uband)
} # }

# Randomly generating a simulated time series
sim <- obs + rnorm(length(obs), mean=3)

if (FALSE) { # \dontrun{
plotbands(obs, lband, uband, sim)
} # }
```
