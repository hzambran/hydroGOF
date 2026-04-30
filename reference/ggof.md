# Graphical Goodness of Fit

Graphical comparison between two vectors (numeric, ts or zoo), with
several numerical goodness of fit printed as a legend.  
Missing values in observed and/or simulated values can removed before
the computations.  

## Usage

``` r
ggof(sim, obs, na.rm = TRUE, dates, date.fmt = "%Y-%m-%d", 
     pt.style = "ts", ftype = "o",  FUN, 
     stype="default", season.names=c("Winter", "Spring", "Summer", "Autumn"),
     gof.leg = TRUE,  digits=2, 
     gofs=c( "ME",  "MAE",  "RMSE", "NRMSE", "PBIAS", "NSE",   "d",    
             "dr",    "r",    "R2",   "KGE",  "LCE", "JDKGE", "VE"),
     legend, leg.cex=1,
     tick.tstep = "auto", lab.tstep = "auto", lab.fmt=NULL,
     cal.ini=NA, val.ini=NA,
     main, xlab = "Time", ylab=c("Q, [m3/s]"),  
     col = c("blue", "black"), 
     cex = c(0.5, 0.5), cex.axis=1.2, cex.lab=1.2,
     lwd = c(1, 1), lty = c(1, 3), pch = c(1, 9), ...)
```

## Arguments

- sim:

  numeric or zoo object with with simulated values

- obs:

  numeric or zoo object with observed values

- na.rm:

  a logical value indicating whether 'NA' should be stripped before the
  computation proceeds.  
  When an 'NA' value is found at the i-th position in `obs` **OR**
  `sim`, the i-th value of `obs` **AND** `sim` are removed before the
  computation.

- dates:

  character, factor, Date or POSIXct object indicating how to obtain the
  dates for the corresponding values in the `sim` and `obs` time
  series  
  If `dates` is a character or factor, it is converted into Date/POSIXct
  class, using the date format specified by `date.fmt`

- date.fmt:

  OPTIONAL. character indicating the format in which the dates are
  stored in `dates`, `cal.ini` and `val.ini`. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html). Default value is
  %Y-%m-%d  
  ONLY required when `class(dates)=="character"` or
  `class(dates)=="factor"` or when `cal.ini` and/or `val.ini` is
  provided.

- pt.style:

  Character indicating if the 2 ts have to be plotted as lines or bars.
  When `ftype` is NOT o, it only applies to the annual values. Valid
  values are:  
  -) ts : (default) each ts is plotted as a lines along the 'x' axis  
  -) bar: both series are plotted as barplots.

- ftype:

  Character indicating how many plots are desired by the user. Valid
  values are:  
  -) o : only the original `sim` and `obs` time series are plotted  
  -) dm : it assumes that `sim` and `obs` are daily time series and
  Daily and Monthly values are plotted  
  -) ma : it assumes that `sim` and `obs` are daily or monthly time
  series and Monthly and Annual values are plotted  
  -) dma : it assumes that `sim` and `obs` are daily time series and
  Daily, Monthly and Annual values are plotted  
  -) seasonal: seasonal values are plotted. See `stype` and
  `season.names`

- FUN:

  OPTIONAL, ONLY required when `ftype` is in
  `c('dm', 'ma', 'dma', 'seasonal')`. Function that have to be applied
  for transforming teh original ts into monthly, annual or seasonal time
  step (e.g., for precipitation FUN MUST be `sum`, for temperature and
  flow time series, FUN MUST be `mean`)

- stype:

  OPTIONAL, only used when `ftype=seasonal`.  
  character, indicating whath weather seasons will be used for computing
  the output. Possible values are:  
  -) default =\> "winter"= DJF = Dec, Jan, Feb; "spring"= MAM = Mar,
  Apr, May; "summer"= JJA = Jun, Jul, Aug; "autumn"= SON = Sep, Oct,
  Nov  
  -) FrenchPolynesia =\> "winter"= DJFM = Dec, Jan, Feb, Mar; "spring"=
  AM = Apr, May; "summer"= JJAS = Jun, Jul, Aug, Sep; "autumn"= ON =
  Oct, Nov

- season.names:

  OPTIONAL, only used when `ftype=seasonal`.  
  character of length 4 indicating the names of each one of the weather
  seasons defined by `stype`.These names are only used for plotting
  purposes

- gof.leg:

  logical, indicating if several numerical goodness of fit have to be
  computed between `sim` and `obs`, and plotted as a legend on the
  graph. If `leg.gof=TRUE`, then `x` is considered as observed and `y`
  as simulated values (for some gof functions this is important).

- digits:

  OPTIONAL, only used when `leg.gof=TRUE`. Numeric, representing the
  decimal places used for rounding the goodness-of-fit indexes.

- gofs:

  character, with one or more strings indicating the goodness-of-fit
  measures to be shown in the legend of the plot when `gof.leg=TRUE`.  
  Possible values when `ftype!='seasonal'` are in
  `c("ME", "MAE", "MSE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE", "VE")`  
  Possible values when `ftype='seasonal'` are in c("ME", "RMSE",
  "PBIAS", "RSR", "NSE", "d", "R2", "KGE", "VE")

- legend:

  character of length 2 to appear in the legend.

- leg.cex:

  OPTIONAL. ONLY used when `leg.gof=TRUE`. Character expansion factor
  for drawing the legend, \*relative\* to current 'par("cex")'. Used for
  text, and provides the default for 'pt.cex' and 'title.cex'. Default
  value = 1

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
  [`drawTimeAxis`](https://hzambran.github.io/hydroTSM/reference/drawxaxis.html).

- cal.ini:

  OPTIONAL. Character, indicating the date in which the calibration
  period started.  
  When `cal.ini` is provided, all the values in `obs` and `sim` with
  dates previous to `cal.ini` are SKIPPED from the computation of the
  goodness-of-fit measures (when `gof.leg=TRUE`), but their values are
  still plotted, in order to examine if the warming up period was too
  short, acceptable or too long for the chosen calibration period. In
  addition, a vertical red line in drawn at this date.

- val.ini:

  OPTIONAL. Character, the date in which the validation period
  started.  
  ONLY used for drawing a vertical red line at this date.

- main:

  character representing the main title of the plot.

- xlab:

  label for the 'x' axis.

- ylab:

  label for the 'y' axis.

- col:

  character, representing the colors of `sim` and `obs`

- cex:

  numeric, representing the values controlling the size of text and
  symbols of 'x' and 'y' with respect to the default

- cex.axis:

  numeric, representing the magnification to be used for the axis
  annotation relative to 'cex'. See
  [`par`](https://rdrr.io/r/graphics/par.html).

- cex.lab:

  numeric, representing the magnification to be used for x and y labels
  relative to the current setting of 'cex'. See
  [`par`](https://rdrr.io/r/graphics/par.html).

- lwd:

  vector with the line width of `sim` and `obs`

- lty:

  numeric with the line type of `sim` and `obs`

- pch:

  numeric with the type of symbol for `x` and `y`. (e.g., 1: white
  circle; 9: white rhombus with a cross inside)

- ...:

  further arguments passed to or from other methods.

## Details

Plots observed and simulated values in the same graph.

If `gof.leg=TRUE`, it computes the numerical values of:  
'me', 'mae', 'rmse', 'nrmse', 'PBIAS', 'RSR, 'rSD', 'NSE', 'mNSE',
'rNSE', 'd', 'md, 'rd', 'cp', 'r', 'r.Spearman', 'R2', 'bR2', 'KGE',
'VE'

## Value

The output of the `gof` function is a matrix with one column only, and
the following rows:

- ME:

  Mean Error

- MAE:

  Mean Absolute Error

- MSE:

  Mean Squared Error

- RMSE:

  Root Mean Square Error

- ubRMSE:

  Unbiased Root Mean Square Error

- NRMSE:

  Normalized Root Mean Square Error ( -100% \<= NRMSE \<= 100% )

- PBIAS:

  Percent Bias ( -Inf \<= PBIAS \<= Inf \[%\] )

- RSR:

  Ratio of RMSE to the Standard Deviation of the Observations, RSR = rms
  / sd(obs). ( 0 \<= RSR \<= +Inf )

- rSD:

  Ratio of Standard Deviations, rSD = sd(sim) / sd(obs)

- NSE:

  Nash-Sutcliffe Efficiency ( -Inf \<= NSE \<= 1 )

- mNSE:

  Modified Nash-Sutcliffe Efficiency ( -Inf \<= mNSE \<= 1 )

- rNSE:

  Relative Nash-Sutcliffe Efficiency ( -Inf \<= rNSE \<= 1 )

- wNSE:

  Weighted Nash-Sutcliffe Efficiency ( -Inf \<= wNSE \<= 1 )

- wsNSE:

  Weighted Seasonal Nash-Sutcliffe Efficiency ( -Inf \<= wsNSE \<= 1 )

- d:

  Index of Agreement ( 0 \<= d \<= 1 )

- dr:

  Refined Index of Agreement ( -1 \<= dr \<= 1 )

- md:

  Modified Index of Agreement ( 0 \<= md \<= 1 )

- rd:

  Relative Index of Agreement ( 0 \<= rd \<= 1 )

- cp:

  Persistence Index ( 0 \<= cp \<= 1 )

- r:

  Pearson Correlation coefficient ( -1 \<= r \<= 1 )

- R2:

  Coefficient of Determination ( 0 \<= R2 \<= 1 )

- bR2:

  R2 multiplied by the coefficient of the regression line between `sim`
  and `obs`  
  ( 0 \<= bR2 \<= 1 )

- VE:

  Volumetric efficiency between `sim` and `obs`  
  ( -Inf \<= VE \<= 1)

- KGE:

  Kling-Gupta efficiency between `sim` and `obs`  
  ( -Inf \<= KGE \<= 1 )

- KGElf:

  Kling-Gupta Efficiency for low values between `sim` and `obs`  
  ( -Inf \<= KGElf \<= 1 )

- KGEnp:

  Non-parametric version of the Kling-Gupta Efficiency between `sim` and
  `obs`  
  ( -Inf \<= KGEnp \<= 1 )

- KGEkm:

  Knowable Moments Kling-Gupta Efficiency between `sim` and `obs`  
  ( -Inf \<= KGEnp \<= 1 )

The following outputs are only produced when both `sim` and `obs` are
zoo objects:

- sKGE:

  Split Kling-Gupta Efficiency between `sim` and `obs`  
  ( -Inf \<= sKGE \<= 1 ). Only computed when both `sim` and `obs` are
  zoo objects

- APFB:

  Annual Peak Flow Bias ( 0 \<= APFB \<= Inf )

- HBF:

  High Flow Bias ( 0 \<= HFB \<= Inf )

- r.Spearman:

  Spearman Correlation coefficient ( -1 \<= r.Spearman \<= 1 ). Only
  computed when `do.spearman=TRUE`

- pbiasfdc:

  PBIAS in the slope of the midsegment of the Flow Duration Curve

## References

Abbaspour, K.C.; Faramarzi, M.; Ghasemi, S.S.; Yang, H. (2009),
Assessing the impact of climate change on water resources in Iran, Water
Resources Research, 45(10), W10,434, doi:10.1029/2008WR007615.

Abbaspour, K.C., Yang, J. ; Maximov, I.; Siber, R.; Bogner, K.;
Mieleitner, J. ; Zobrist, J.; Srinivasan, R. (2007), Modelling hydrology
and water quality in the pre-alpine/alpine Thur watershed using SWAT,
Journal of Hydrology, 333(2-4), 413-430,
doi:10.1016/j.jhydrol.2006.09.014.

Box, G.E. (1966). Use and abuse of regression. Technometrics, 8(4),
625-629. doi:10.1080/00401706.1966.10490407.

Barrett, J.P. (1974). The coefficient of determination-some limitations.
The American Statistician, 28(1), 19-20.
doi:10.1080/00031305.1974.10479056.

Chai, T.; Draxler, R.R. (2014). Root mean square error (RMSE) or mean
absolute error (MAE)? - Arguments against avoiding RMSE in the
literature, Geoscientific Model Development, 7, 1247-1250.
doi:10.5194/gmd-7-1247-2014.

Cinkus, G.; Mazzilli, N.; Jourde, H.; Wunsch, A.; Liesch, T.; Ravbar,
N.; Chen, Z.; and Goldscheider, N. (2023). When best is the enemy of
good - critical evaluation of performance criteria in hydrological
models. Hydrology and Earth System Sciences 27, 2397-2411,
doi:10.5194/hess-27-2397-2023.

Criss, R. E.; Winston, W. E. (2008), Do Nash values have value?
Discussion and alternate proposals. Hydrological Processes, 22:
2723-2725. doi:10.1002/hyp.7072.

Entekhabi, D.; Reichle, R.H.; Koster, R.D.; Crow, W.T. (2010).
Performance metrics for soil moisture retrievals and application
requirements. Journal of Hydrometeorology, 11(3), 832-840. doi:
10.1175/2010JHM1223.1.

Ficchi, A.; Bavera, D.; Grimaldi, S.; Moschini, F.; Pistocchi, A.;
Russo, C.; Salamon, P.; Toreti, A. (2026). Improving low and high flow
simulations at once: An enhanced metric for hydrological model
calibrations. EGUsphere \[preprint\],
https://doi.org/10.5194/egusphere-2026-43.

Fowler, K.; Coxon, G.; Freer, J.; Peel, M.; Wagener, T.; Western, A.;
Woods, R.; Zhang, L. (2018). Simulating runoff under changing climatic
conditions: A framework for model improvement. Water Resources Research,
54(12), 812-9832. doi:10.1029/2018WR023989.

Garcia, F.; Folton, N.; Oudin, L. (2017). Which objective function to
calibrate rainfall-runoff models for low-flow index simulations?.
Hydrological sciences journal, 62(7), 1149-1166.
doi:10.1080/02626667.2017.1308511.

Garrick, M.; Cunnane, C.; Nash, J.E. (1978). A criterion of efficiency
for rainfall-runoff models. Journal of Hydrology 36, 375-381.
doi:10.1016/0022-1694(78)90155-5.

Gupta, H.V.; Kling, H.; Yilmaz, K.K.; Martinez, G.F. (2009).
Decomposition of the mean squared error and NSE performance criteria:
Implications for improving hydrological modelling. Journal of hydrology,
377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694.

Gupta, H.V.; Kling, H. (2011). On typical range, sensitivity, and
normalization of Mean Squared Error and Nash-Sutcliffe Efficiency type
metrics. Water Resources Research, 47(10). doi:10.1029/2011WR010962.

Hahn, G.J. (1973). The coefficient of determination exposed. Chemtech,
3(10), 609-612. Aailable online at:
<https://www2.hawaii.edu/~cbaajwe/Ph.D.Seminar/Hahn1973.pdf>.

Hodson, T.O. (2022). Root-mean-square error (RMSE) or mean absolute
error (MAE): when to use them or not, Geoscientific Model Development,
15, 5481-5487, doi:10.5194/gmd-15-5481-2022.

Hundecha, Y., Bardossy, A. (2004). Modeling of the effect of land use
changes on the runoff generation of a river basin through parameter
regionalization of a watershed model. Journal of hydrology, 292(1-4),
281-295. doi:10.1016/j.jhydrol.2004.01.002.

Kitanidis, P.K.; Bras, R.L. (1980). Real-time forecasting with a
conceptual hydrologic model. 2. Applications and results. Water
Resources Research, Vol. 16, No. 6, pp. 1034:1044.
doi:10.1029/WR016i006p01034.  

Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper
Danube basin under an ensemble of climate change scenarios. Journal of
Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.

Knoben, W.J.; Freer, J.E.; Woods, R.A. (2019). Inherent benchmark or
not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores.
Hydrology and Earth System Sciences, 23(10), 4323-4331.
doi:10.5194/hess-23-4323-2019.

Krause, P.; Boyle, D.P.; Base, F. (2005). Comparison of different
efficiency criteria for hydrological model assessment, Advances in
Geosciences, 5, 89-97. doi:10.5194/adgeo-5-89-2005.  

Krstic, G.; Krstic, N.S.; Zambrano-Bigiarini, M. (2016). The
br2-weighting Method for Estimating the Effects of Air Pollution on
Population Health. Journal of Modern Applied Statistical Methods, 15(2),
42. doi:10.22237/jmasm/1478004000

Lee, J. S.; Choi, H. I. (2022). A rebalanced performance criterion for
hydrological model calibration. Journal of Hydrology, 606, 127372.
https://doi.org/10.1016/j.jhydrol.2021.127372

Legates, D.R.; McCabe, G. J. Jr. (1999), Evaluating the Use of
"Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model
Validation, Water Resour. Res., 35(1), 233-241.
doi:10.1029/1998WR900018.

Ling, X.; Huang, Y.; Guo, W.; Wang, Y.; Chen, C.; Qiu, B.; Ge, J.; Qin,
K.; Xue, Y.; Peng, J. (2021). Comprehensive evaluation of
satellite-based and reanalysis soil moisture products using in situ
observations over China. Hydrology and Earth System Sciences, 25(7),
4209-4229. doi:10.5194/hess-25-4209-2021.

Liu, D.; Chen, X.; Lian, Y.; Lou, Z. (2020). A new performance measure
for hydrologic models. Journal of Hydrology, 590, 125488.
doi:10.1016/j.jhydrol.2020.125488.

Mizukami, N.; Rakovec, O.; Newman, A.J.; Clark, M.P.; Wood, A.W.; Gupta,
H.V.; Kumar, R.: (2019). On the choice of calibration metrics for
"high-flow" estimation using hydrologic models, Hydrology Earth System
Sciences 23, 2601-2614, doi:10.5194/hess-23-2601-2019.

Moriasi, D.N.; Arnold, J.G.; van Liew, M.W.; Bingner, R.L.; Harmel,
R.D.; Veith, T.L. (2007). Model evaluation guidelines for systematic
quantification of accuracy in watershed simulations. Transactions of the
ASABE. 50(3):885-900

Nash, J.E. and Sutcliffe, J.V. (1970). River flow forecasting through
conceptual models. Part 1: a discussion of principles, Journal of
Hydrology 10, pp. 282-290. doi:10.1016/0022-1694(70)90255-6.

Pearson, K. (1920). Notes on the history of correlation. Biometrika,
13(1), 25-45. doi:10.2307/2331722.

Pfannerstill, M.; Guse, B.; Fohrer, N. (2014). Smart low flow signature
metrics for an improved overall performance evaluation of hydrological
models. Journal of Hydrology, 510, 447-458.
doi:10.1016/j.jhydrol.2013.12.044.

Pizarro, A.; Jorquera, J. (2024). Advancing objective functions in
hydrological modelling: Integrating knowable moments for improved
simulation accuracy. Journal of Hydrology, 634, 131071.
doi:10.1016/j.jhydrol.2024.131071.

Pool, S.; Vis, M.; Seibert, J. (2018). Evaluating model performance:
towards a non-parametric variant of the Kling-Gupta efficiency.
Hydrological Sciences Journal, 63(13-14), pp.1941-1953.
doi:/10.1080/02626667.2018.1552002.

Pushpalatha, R.; Perrin, C.; Le Moine, N.; Andreassian, V. (2012). A
review of efficiency criteria suitable for evaluating low-flow
simulations. Journal of Hydrology, 420, 171-182.
doi:10.1016/j.jhydrol.2011.11.055.

Royer-Gaspard, P., Andreassian, V., and Thirel, G. (2021). Technical
note: PMR - a proxy metric to assess hydrological model robustness in a
changing climate. Hydrology and Earth System Sciences, 25, 5703–5716.
doi:10.5194/hess-25-5703-2021.

Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using
log-transformed flows within the KGE criterion.
doi:10.5194/hess-22-4583-2018.

Schaefli, B., Gupta, H. (2007). Do Nash values have value?. Hydrological
Processes 21, 2075-2080. doi:10.1002/hyp.6825.

Schober, P.; Boer, C.; Schwarte, L.A. (2018). Correlation coefficients:
appropriate use and interpretation. Anesthesia and Analgesia, 126(5),
1763-1768. doi:10.1213/ANE.0000000000002864.

Schuol, J.; Abbaspour, K.C.; Srinivasan, R.; Yang, H. (2008b),
Estimation of freshwater availability in the West African sub-continent
using the SWAT hydrologic model, Journal of Hydrology, 352(1-2), 30,
doi:10.1016/j.jhydrol.2007.12.025

Sorooshian, S., Q. Duan, and V. K. Gupta. (1993). Calibration of
rainfall-runoff models: Application of global optimization to the
Sacramento Soil Moisture Accounting Model, Water Resources Research, 29
(4), 1185-1194, doi:10.1029/92WR02617.

Spearman, C. (1961). The Proof and Measurement of Association Between
Two Things. In J. J. Jenkins and D. G. Paterson (Eds.), Studies in
individual differences: The search for intelligence (pp. 45-58).
Appleton-Century-Crofts. doi:10.1037/11491-005

Tang, G.; Clark, M.P.; Papalexiou, S.M. (2021). SC-earth: a
station-based serially complete earth dataset from 1950 to 2019. Journal
of Climate, 34(16), 6493-6511. doi:10.1175/JCLI-D-21-0067.1.

Yapo P.O.; Gupta H.V.; Sorooshian S. (1996). Automatic calibration of
conceptual rainfall-runoff models: sensitivity to calibration data.
Journal of Hydrology. v181 i1-4. 23-48. doi:10.1016/0022-1694(95)02918-4

Yilmaz, K.K., Gupta, H.V. ; Wagener, T. (2008), A process-based
diagnostic approach to model evaluation: Application to the NWS
distributed hydrologic model, Water Resources Research, 44, W09417,
doi:10.1029/2007WR006716.

Willmott, C.J. (1981). On the validation of models. Physical Geography,
2, 184–194. doi:10.1080/02723646.1981.10642213.

Willmott, C.J. (1984). On the evaluation of model performance in
physical geography. Spatial Statistics and Models, G. L. Gaile and C. J.
Willmott, eds., 443-460. doi:10.1007/978-94-017-3048-8_23.

Willmott, C.J.; Ackleson, S.G. Davis, R.E.; Feddema, J.J.; Klink, K.M.;
Legates, D.R.; O'Donnell, J.; Rowe, C.M. (1985), Statistics for the
Evaluation and Comparison of Models, J. Geophys. Res., 90(C5),
8995-9005. doi:10.1029/JC090iC05p08995.

Willmott, C.J.; Matsuura, K. (2005). Advantages of the mean absolute
error (MAE) over the root mean square error (RMSE) in assessing average
model performance, Climate Research, 30, 79-82, doi:10.3354/cr030079.

Willmott, C.J.; Matsuura, K.; Robeson, S.M. (2009). Ambiguities inherent
in sums-of-squares-based error statistics, Atmospheric Environment, 43,
749-752, doi:10.1016/j.atmosenv.2008.10.005.

Willmott, C.J.; Robeson, S.M.; Matsuura, K. (2012). A refined index of
model performance. International Journal of climatology, 32(13),
pp.2088-2094. doi:10.1002/joc.2419.

Willmott, C.J.; Robeson, S.M.; Matsuura, K.; Ficklin, D.L. (2015).
Assessment of three dimensionless measures of model performance.
Environmental Modelling & Software, 73, pp.167-174.
doi:10.1016/j.envsoft.2015.08.012

Zambrano-Bigiarini, M.; Bellin, A. (2012). Comparing goodness-of-fit
measures for calibration of models focused on extreme events. EGU
General Assembly 2012, Vienna, Austria, 22-27 Apr 2012, EGU2012-11549-1.

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

## See also

[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`plot2`](https://hzambran.github.io/hydroGOF/reference/plot2.md),
`ggof`, [`me`](https://hzambran.github.io/hydroGOF/reference/me.md),
[`mae`](https://hzambran.github.io/hydroGOF/reference/mae.md),
[`mse`](https://hzambran.github.io/hydroGOF/reference/mse.md),
[`rmse`](https://hzambran.github.io/hydroGOF/reference/rmse.md),
[`ubRMSE`](https://hzambran.github.io/hydroGOF/reference/ubRMSE.md),
[`nrmse`](https://hzambran.github.io/hydroGOF/reference/nrmse.md),
[`pbias`](https://hzambran.github.io/hydroGOF/reference/pbias.md),
[`rsr`](https://hzambran.github.io/hydroGOF/reference/rsr.md),
[`rSD`](https://hzambran.github.io/hydroGOF/reference/rSD.md),
[`NSE`](https://hzambran.github.io/hydroGOF/reference/NSE.md),
[`mNSE`](https://hzambran.github.io/hydroGOF/reference/mNSE.md),
[`rNSE`](https://hzambran.github.io/hydroGOF/reference/rNSE.md),
[`wNSE`](https://hzambran.github.io/hydroGOF/reference/wNSE.md),
[`d`](https://hzambran.github.io/hydroGOF/reference/d.md),
[`dr`](https://hzambran.github.io/hydroGOF/reference/dr.md),
[`md`](https://hzambran.github.io/hydroGOF/reference/md.md),
[`rd`](https://hzambran.github.io/hydroGOF/reference/rd.md),
[`cp`](https://hzambran.github.io/hydroGOF/reference/cp.md),
[`rPearson`](https://hzambran.github.io/hydroGOF/reference/rPearson.md),
[`R2`](https://hzambran.github.io/hydroGOF/reference/R2.md),
[`br2`](https://hzambran.github.io/hydroGOF/reference/br2.md),
[`KGE`](https://hzambran.github.io/hydroGOF/reference/KGE.md),
[`KGElf`](https://hzambran.github.io/hydroGOF/reference/KGElf.md),
[`KGEnp`](https://hzambran.github.io/hydroGOF/reference/KGEnp.md),
[`sKGE`](https://hzambran.github.io/hydroGOF/reference/sKGE.md),
[`VE`](https://hzambran.github.io/hydroGOF/reference/VE.md),
[`rSpearman`](https://hzambran.github.io/hydroGOF/reference/rSpearman.md),
[`pbiasfdc`](https://hzambran.github.io/hydroGOF/reference/pbiasfdc.md)

## Examples

``` r
obs <- 1:10
sim <- 2:11

if (FALSE) { # \dontrun{
ggof(sim, obs)
} # }

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Getting the numeric goodness of fit for the "best" (unattainable) case
gof(sim=sim, obs=obs)
#>         [,1]
#> ME         0
#> MAE        0
#> MSE        0
#> RMSE       0
#> ubRMSE     0
#> NRMSE %    0
#> PBIAS %    0
#> RSR        0
#> rSD        1
#> NSE        1
#> mNSE       1
#> rNSE       1
#> wNSE       1
#> wsNSE      1
#> d          1
#> dr         1
#> md         1
#> rd         1
#> cp         1
#> r          1
#> R2         1
#> bR2        1
#> VE         1
#> KGE        0
#> KGElf      0
#> KGEnp      1
#> KGEkm      1
#> JDKGE      1
#> LME        1
#> LCE        1
#> sKGE       0
#> APFB       0
#> HFB        1

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Getting the new numeric goodness-of-fit measures
gof(sim=sim, obs=obs)
#>          [,1]
#> ME       5.46
#> MAE      5.46
#> MSE     55.01
#> RMSE     7.42
#> ubRMSE   5.02
#> NRMSE % 37.10
#> PBIAS % 34.50
#> RSR      0.37
#> rSD      1.04
#> NSE      0.86
#> mNSE     0.57
#> rNSE    -0.56
#> wNSE     0.97
#> wsNSE    0.72
#> d        0.97
#> dr       0.78
#> md       0.78
#> rd       0.63
#> cp       0.42
#> r        0.97
#> R2       0.86
#> bR2      0.76
#> VE       0.65
#> KGE     -0.35
#> KGElf   -0.09
#> KGEnp    0.61
#> KGEkm    0.63
#> JDKGE    0.64
#> LME      0.65
#> LCE      0.65
#> sKGE    -0.37
#> APFB     0.03
#> HFB      0.91

# Getting the graphical representation of 'obs' and 'sim' along with the numeric 
# goodness-of-fit measures for the daily and monthly time series 
if (FALSE) { # \dontrun{
ggof(sim=sim, obs=obs, ftype="dm", FUN=mean)
} # }

# Getting the graphical representation of 'obs' and 'sim' along with some numeric 
# goodness-of-fit measures for the seasonal time series 
if (FALSE) { # \dontrun{
ggof(sim=sim, obs=obs, ftype="seasonal", FUN=mean)
} # }

# Computing the daily residuals 
# even if this is a dummy example, it is enough for illustrating the capability
r <- sim-obs

# Summarizing and plotting the residuals
if (FALSE) { # \dontrun{
library(hydroTSM)

# summary
smry(r) 

# daily, monthly and annual plots, boxplots and histograms
hydroplot(r, FUN=mean)

# seasonal plots and boxplots
hydroplot(r, FUN=mean, pfreq="seasonal")
} # }
```
