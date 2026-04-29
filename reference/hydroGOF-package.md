# Goodness-of-fit (GoF) functions for numerical and graphical comparison of simulated and observed time series, mainly focused on hydrological modelling.

S3 functions implementing both statistical and graphical goodness-of-fit
measures between observed and simulated values, to be used during the
calibration, validation, and application of hydrological models.  

Missing values in observed and/or simulated values can be removed before
computations.  

## Details

|             |                                                                          |
|-------------|--------------------------------------------------------------------------|
| Package:    | hydroGOF                                                                 |
| Type:       | Package                                                                  |
| Version:    | 0.6-0                                                                    |
| Date:       | 2024-05-08                                                               |
| License:    | GPL \>= 2                                                                |
| LazyLoad:   | yes                                                                      |
| Packaged:   | Wed 08 May 2024 05:13:53 PM -04 ; MZB                                    |
| BuiltUnder: | R version 4.4.0 (2024-04-24) – "Puppy Cup" ;x86_64-pc-linux-gnu (64-bit) |

Quantitative statistics included in this package are:

|                                                                                                                                         |                                                                                                                                               |
|-----------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| [`me`](https://hzambran.github.io/hydroGOF/reference/me.md) Mean Error                                                                  | [`mae`](https://hzambran.github.io/hydroGOF/reference/mae.md) Mean Absolute Error                                                             |
| [`mse`](https://hzambran.github.io/hydroGOF/reference/mse.md) Mean Squared Error                                                        | [`rmse`](https://hzambran.github.io/hydroGOF/reference/rmse.md) Root Mean Square Error                                                        |
| [`ubRMSE`](https://hzambran.github.io/hydroGOF/reference/ubRMSE.md) Unbiased Root Mean Square Error                                     | [`nrmse`](https://hzambran.github.io/hydroGOF/reference/nrmse.md) Normalized Root Mean Square Error                                           |
| [`pbias`](https://hzambran.github.io/hydroGOF/reference/pbias.md) Percent Bias                                                          | [`rsr`](https://hzambran.github.io/hydroGOF/reference/rsr.md) Ratio of RMSE to the Standard Deviation of the Observations                     |
| [`rSD`](https://hzambran.github.io/hydroGOF/reference/rSD.md) Ratio of Standard Deviations                                              | [`NSE`](https://hzambran.github.io/hydroGOF/reference/NSE.md) Nash-Sutcliffe Efficiency                                                       |
| [`mNSE`](https://hzambran.github.io/hydroGOF/reference/mNSE.md) Modified Nash-Sutcliffe Efficiency                                      | [`rNSE`](https://hzambran.github.io/hydroGOF/reference/rNSE.md) Relative Nash-Sutcliffe Efficiency                                            |
| [`wNSE`](https://hzambran.github.io/hydroGOF/reference/wNSE.md) Weighted Nash-Sutcliffe Efficiency                                      | [`wsNSE`](https://hzambran.github.io/hydroGOF/reference/wsNSE.md) Weighted Seasonal Nash-Sutcliffe Efficiency                                 |
| [`d`](https://hzambran.github.io/hydroGOF/reference/d.md) Index of Agreement                                                            | [`dr`](https://hzambran.github.io/hydroGOF/reference/dr.md) Refined Index of Agreement                                                        |
| [`md`](https://hzambran.github.io/hydroGOF/reference/md.md) Modified Index of Agreement                                                 | [`rd`](https://hzambran.github.io/hydroGOF/reference/rd.md) Relative Index of Agreement                                                       |
| [`cp`](https://hzambran.github.io/hydroGOF/reference/cp.md) Persistence Index                                                           | [`rPearson`](https://hzambran.github.io/hydroGOF/reference/rPearson.md) Pearson correlation coefficient                                       |
| [`R2`](https://hzambran.github.io/hydroGOF/reference/R2.md) Coefficient of determination                                                | [`br2`](https://hzambran.github.io/hydroGOF/reference/br2.md) R2 multiplied by the coefficient of the regression line between `sim` and `obs` |
| [`VE`](https://hzambran.github.io/hydroGOF/reference/VE.md) Volumetric efficiency                                                       | [`KGE`](https://hzambran.github.io/hydroGOF/reference/KGE.md) Kling-Gupta efficiency                                                          |
| [`KGElf`](https://hzambran.github.io/hydroGOF/reference/KGElf.md) Kling-Gupta Efficiency for low values                                 | [`KGEnp`](https://hzambran.github.io/hydroGOF/reference/KGEnp.md) Non-parametric version of the Kling-Gupta Efficiency                        |
| [`KGEkm`](https://hzambran.github.io/hydroGOF/reference/KGEkm.md) Knowable Moments Kling-Gupta Efficiency                               | [`sKGE`](https://hzambran.github.io/hydroGOF/reference/sKGE.md) Split Kling-Gupta Efficiency                                                  |
| [`APFB`](https://hzambran.github.io/hydroGOF/reference/APFB.md) Annual Peak Flow Bias                                                   | [`HFB`](https://hzambran.github.io/hydroGOF/reference/HFB.md) High Flow Bias                                                                  |
| [`rSpearman`](https://hzambran.github.io/hydroGOF/reference/rSpearman.md) Spearman's rank correlation coefficient                       | [`ssq`](https://hzambran.github.io/hydroGOF/reference/ssq.md) Sum of the Squared Residuals                                                    |
| [`pbiasfdc`](https://hzambran.github.io/hydroGOF/reference/pbiasfdc.md) PBIAS in the slope of the midsegment of the flow duration curve | [`pfactor`](https://hzambran.github.io/hydroGOF/reference/pfactor.md) P-factor                                                                |
| [`rfactor`](https://hzambran.github.io/hydroGOF/reference/rfactor.md) R-factor                                                          | ———————————————————————————————————-                                                                                                          |

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

Maintainer: Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

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

Legates, D.R.; McCabe, G. J. Jr. (1999), Evaluating the Use of
"Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model
Validation, Water Resour. Res., 35(1), 233-241.
doi:10.1029/1998WR900018.

Ling, X.; Huang, Y.; Guo, W.; Wang, Y.; Chen, C.; Qiu, B.; Ge, J.; Qin,
K.; Xue, Y.; Peng, J. (2021). Comprehensive evaluation of
satellite-based and reanalysis soil moisture products using in situ
observations over China. Hydrology and Earth System Sciences, 25(7),
4209-4229. doi:10.5194/hess-25-4209-2021.

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

## See also

<https://CRAN.R-project.org/package=hydroPSO>  
<https://CRAN.R-project.org/package=hydroTSM>

## Examples

``` r
obs <- 1:100
sim <- obs

# Numerical goodness of fit
gof(sim,obs)
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

# Reverting the order of simulated values
sim <- 100:1
gof(sim,obs)
#>             [,1]
#> ME          0.00
#> MAE        50.00
#> MSE      3333.00
#> RMSE       57.73
#> ubRMSE     57.73
#> NRMSE %   199.00
#> PBIAS %     0.00
#> RSR         1.99
#> rSD         1.00
#> NSE        -3.00
#> mNSE       -1.00
#> rNSE     -457.56
#> wNSE       -3.00
#> wsNSE      -1.00
#> d           0.00
#> dr          0.00
#> md          0.00
#> rd       -113.64
#> cp      -3266.67
#> r          -1.00
#> R2         -3.00
#> bR2        -1.52
#> VE          0.01
#> KGE        -1.24
#> KGElf      -0.87
#> KGEnp      -1.00
#> KGEkm      -1.00

if (FALSE) { # \dontrun{
ggof(sim, obs)
} # }

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to observations
sim <- obs 

# Getting the numeric goodness-of-fit measures for the "best" (unattainable) case
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
#> sKGE       0
#> APFB       0
#> HFB        1

# Randomly changing the first 2000 elements of 'sim', by using a normal 
# distribution  with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Getting the new numeric goodness of fit
gof(sim=sim, obs=obs)
#>          [,1]
#> ME       5.46
#> MAE      5.46
#> MSE     55.05
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
#> sKGE    -0.37
#> APFB     0.03
#> HFB      0.92

# Graphical representation of 'obs' vs 'sim', along with the numeric 
# goodness-of-fit measures
if (FALSE) { # \dontrun{
ggof(sim=sim, obs=obs)
} # }
```
