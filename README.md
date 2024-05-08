# hydroGOF
[![Research software impact](http://depsy.org/api/package/cran/hydroGOF/badge.svg)](http://depsy.org/package/r/hydroGOF) [![CRAN](http://www.r-pkg.org/badges/version/hydroGOF)](https://cran.r-project.org/package=hydroGOF) [![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) [![monthly](http://cranlogs.r-pkg.org/badges/hydroGOF)](https://www.rpackages.io/package/hydroGOF) [![total](http://cranlogs.r-pkg.org/badges/grand-total/hydroGOF)](https://www.rpackages.io/package/hydroGOF) [![Build Status](https://travis-ci.org/hzambran/hydroGOF.svg?branch=master)](https://travis-ci.org/hzambran/hydroGOF) [![dependencies](https://tinyverse.netlify.com/badge/hydroGOF)](https://CRAN.R-project.org/package=hydroGOF)

<!-- badges: start -->
  [![R-CMD-check](https://github.com/hzambran/hydroGOF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hzambran/hydroGOF/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

hydroGOF is an R package that provides S3 functions implementing both statistical and graphical goodness-of-fit measures between observed and simulated values, mainly oriented to be used during the calibration, validation, and application of hydrological models.

Missing values in observed and/or simulated values can be automatically removed before the computations.

Bugs / comments / questions / collaboration of any kind are very welcomed. 



## Installation
Installing the latest stable version from [CRAN](https://CRAN.R-project.org/package=hydroGOF):
```{r}
install.packages("hydroGOF")
```

Alternatively, you can also try the under-development version from [Github](https://github.com/hzambran/hydroGOF):
```{r}
if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("hzambran/hydroGOF")
```


## Reporting bugs, requesting new features

If you find an error in some function, or want to report a typo in the documentation, or to request a new feature (and wish it be implemented :) you can do it [here](https://github.com/hzambran/hydroGOF/issues)


## Citation 
```{r}
citation("hydroGOF")
```

To cite hydroGOF in publications use:

> Zambrano-Bigiarini, Mauricio (2024). hydroGOF: Goodness-of-fit functions for comparison of simulated and observed hydrological time series. R package version 0.6-0. URL:https://cran.r-project.org/package=hydroGOF. doi:10.5281/zenodo.839854.


A BibTeX entry for LaTeX users is

>  @Manual{hydroGOF,  
>    title = {hydroGOF: Goodness-of-fit functions for comparison of simulated and observed hydrological time series},  
>    author = {Zambrano-Bigiarini, Mauricio},  
>    note = {R package version 0.6-0},  
>    year = {2024},
>    url = {https://cran.r-project.org/package=hydroGOF},  
>    doi = {10.5281/zenodo.839854},  
>  }

## Goodness-of-fit measures

Quantitative statistics included in this package are: 

* me:         Mean Error (**Hill et al., 2006**)
* mae:        Mean Absolute Error (**Hodson, 2022**)  
* mse:        Mean Squared Error (**Yapo et al., 1996**)
* rmse:       Root Mean Square Error (**Willmott and Matsuura, 2005**)
* ubRMSE:     Unbiased Root Mean Square Error (**Entekhabi et al., 2010**) 
* nrmse:      Normalized Root Mean Square Error 
* pbias:      Percent Bias (**Yapo et al., 1996**)
* rsr:        Ratio of RMSE to the Standard Deviation of the Observations (**Moriasi et al., 2007**)
* rSD:        Ratio of Standard Deviations 
* NSE:        Nash-Sutcliffe Efficiency (**Nash and Sutcliffe, 1970**)
* mNSE:       Modified Nash-Sutcliffe Efficiency (**Krause et al., 2005**)
* rNSE:       Relative Nash-Sutcliffe Efficiency (**Legates and McCabe, 1999**)
* wNSE:       Weighted Nash-Sutcliffe Efficiency (**Hundecha and Bardossy, 2004**)
* wsNSE:      Weighted Seasonal Nash-Sutcliffe Efficiency (**Zambrano-Bigiarini and Bellin, A., 2012**)
* d:          Index of Agreement (**Willmott, C.J., 1981**)
* dr:         Refined Index of Agreement (**Willmott et al., 2012**)
* md:         Modified Index of Agreement (**Krause et al., 2005**) 
* rd:         Relative Index of Agreement (**Krause et al., 2005**) 
* cp:         Persistence Index (**Kitanidis and Bras, 1980**)
* rPearson:   Pearson correlation coefficient (**Pearson, 1920**)
* R2:         Coefficient of determination (**Box, 1966**)
* br2:        R2 multiplied by the coefficient of the regression line between \code{sim} and \code{obs} (**Krause et al., 2005**)
* VE:         Volumetric efficiency (**Criss and Winston, 2008**) 
* KGE:        Kling-Gupta efficiency (**Gupta et al., 2009**)
* KGElf:      Kling-Gupta Efficiency for low values (**Garcia et al., 2017**)
* KGEnp:      Non-parametric version of the Kling-Gupta Efficiency (**Pool et al., 2018**)
* KGEkm:      Knowable Moments Kling-Gupta Efficiency (**Pizarro and Jorquera, 2024**)
* sKGE:       Split Kling-Gupta Efficiency (**Fowler et al., 2018**)
* APFB:       Annual Peak Flow Bias (**Mizukami et al., 2019**)
* HFB:        High Flow Bias 
* rSpearman:  Spearman's rank correlation coefficient (**Spearman, 1961**) 
* ssq:        Sum of the Squared Residuals (**Willmott et al., 2009**)
* pbiasfdc:   PBIAS in the slope of the midsegment of the flow duration curve (**Yilmaz et al., 2008**)
* pfactor:    P-factor (**Abbaspour et al., 2009**)
* rfactor:    R-factor (**Abbaspour et al., 2009**)


## References

* Abbaspour, K.C.; Faramarzi, M.; Ghasemi, S.S.; Yang, H. (2009), [Assessing the impact of climate change on water resources in Iran](https://doi.org/10.1029/2008WR007615), Water Resources Research, 45(10), W10,434, doi:10.1029/2008WR007615. 

* Abbaspour, K.C., Yang, J. ; Maximov, I.; Siber, R.; Bogner, K.; Mieleitner, J. ; Zobrist, J.; Srinivasan, R. (2007), [Modelling hydrology and water quality in the pre-alpine/alpine Thur watershed using SWAT](https://doi.org/10.1016/j.jhydrol.2006.09.014), Journal of Hydrology, 333(2-4), 413-430, doi:10.1016/j.jhydrol.2006.09.014. 

* Box, G.E. (1966). [Use and abuse of regression](https://doi.org/10.1080/00401706.1966.10490407). Technometrics, 8(4), 625-629. doi:10.1080/00401706.1966.10490407. 

* Barrett, J.P. (1974). [The coefficient of determination-some limitations](https://doi.org/10.1080/00031305.1974.10479056). The American Statistician, 28(1), 19-20. doi:10.1080/00031305.1974.10479056. 

* Chai, T.; Draxler, R.R. (2014). [Root mean square error (RMSE) or mean absolute error (MAE)? - Arguments against avoiding RMSE in the literature](https://doi.org/10.5194/gmd-7-1247-2014), Geoscientific Model Development, 7, 1247-1250. doi:10.5194/gmd-7-1247-2014. 

* Cinkus, G.; Mazzilli, N.; Jourde, H.; Wunsch, A.; Liesch, T.; Ravbar, N.; Chen, Z.; and Goldscheider, N. (2023). [When best is the enemy of good - critical evaluation of performance criteria in hydrological models](https://doi.org/10.5194/hess-27-2397-2023). Hydrology and Earth System Sciences 27, 2397-2411, doi:10.5194/hess-27-2397-2023. 

* Criss, R. E.; Winston, W. E. (2008), [Do Nash values have value? Discussion and alternate proposals](https://doi.org/10.1002/hyp.7072). Hydrological Processes, 22: 2723-2725. doi:10.1002/hyp.7072. 

* Entekhabi, D.; Reichle, R.H.; Koster, R.D.; Crow, W.T. (2010). [Performance metrics for soil moisture retrievals and application requirements](https://doi.org/10.1175/2010JHM1223.1). Journal of Hydrometeorology, 11(3), 832-840. doi:10.1175/2010JHM1223.1. 

* Fowler, K.; Coxon, G.; Freer, J.; Peel, M.; Wagener, T.; Western, A.; Woods, R.; Zhang, L. (2018). [Simulating runoff under changing climatic conditions: A framework for model improvement](https://doi.org/10.1029/2018WR023989). Water Resources Research, 54(12), 812-9832. doi:10.1029/2018WR023989. 

* Garcia, F.; Folton, N.; Oudin, L. (2017). [Which objective function to calibrate rainfall-runoff models for low-flow index simulations?](https://doi.org/10.1080/02626667.2017.1308511). Hydrological sciences journal, 62(7), 1149-1166. doi:10.1080/02626667.2017.1308511. 

* Garrick, M.; Cunnane, C.; Nash, J.E. (1978). [A criterion of efficiency for rainfall-runoff models](https://doi.org/10.1016/0022-1694(78)90155-5). Journal of Hydrology 36, 375-381. doi:10.1016/0022-1694(78)90155-5. 

* Gupta, H.V.; Kling, H.; Yilmaz, K.K.; Martinez, G.F. (2009). [Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling](https://doi.org/10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694). Journal of hydrology, 377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694. 

* Gupta, H.V.; Kling, H. (2011). [On typical range, sensitivity, and normalization of Mean Squared Error and Nash-Sutcliffe Efficiency type metrics](https://doi.org/10.1029/2011WR010962). Water Resources Research, 47(10). doi:10.1029/2011WR010962. 

* Hahn, G.J. (1973). [The coefficient of determination exposed](https://www2.hawaii.edu/~cbaajwe/Ph.D.Seminar/Hahn1973.pdf). Chemtech, 3(10), 609-612. Aailable online at: \url{https://www2.hawaii.edu/~cbaajwe/Ph.D.Seminar/Hahn1973.pdf}. 

* Hodson, T.O. (2022). [Root-mean-square error (RMSE) or mean absolute error (MAE): when to use them or not](https://doi.org/10.5194/gmd-15-5481-2022), Geoscientific Model Development, 15, 5481-5487, doi:10.5194/gmd-15-5481-2022. 

* Hundecha, Y., Bardossy, A. (2004). [Modeling of the effect of land use changes on the runoff generation of a river basin through parameter regionalization of a watershed model](https://doi.org/10.1016/j.jhydrol.2004.01.002). Journal of hydrology, 292(1-4), 281-295. doi:10.1016/j.jhydrol.2004.01.002. 

* Kitanidis, P.K.; Bras, R.L. (1980). [Real-time forecasting with a conceptual hydrologic model](https://doi.org/10.1029/WR016i006p01034). 2. Applications and results. Water Resources Research, Vol. 16, No. 6, pp. 1034:1044. doi:10.1029/WR016i006p01034.

* Kling, H.; Fuchs, M.; Paulin, M. (2012). [Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios](https://doi.org/10.1016/j.jhydrol.2012.01.011). Journal of Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011. 

* Knoben, W.J.; Freer, J.E.; Woods, R.A. (2019). [Inherent benchmark or not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores](https://doi.org/10.5194/hess-23-4323-2019). Hydrology and Earth System Sciences, 23(10), 4323-4331. doi:10.5194/hess-23-4323-2019. 

* Krause, P.; Boyle, D.P.; Base, F. (2005). [Comparison of different efficiency criteria for hydrological model assessment](https://doi.org/10.5194/adgeo-5-89-2005), Advances in Geosciences, 5, 89-97. doi:10.5194/adgeo-5-89-2005.

* Krstic, G.; Krstic, N.S.; Zambrano-Bigiarini, M. (2016). [The br2-weighting Method for Estimating the Effects of Air Pollution on Population Health](https://doi.org/10.22237/jmasm/1478004000). Journal of Modern Applied Statistical Methods, 15(2), 42. doi:10.22237/jmasm/1478004000.

* Legates, D.R.; McCabe, G. J. Jr. (1999), [Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation](https://doi.org/10.1029/1998WR900018), Water Resour. Res., 35(1), 233-241. doi:10.1029/1998WR900018.

* Ling, X.; Huang, Y.; Guo, W.; Wang, Y.; Chen, C.; Qiu, B.; Ge, J.; Qin, K.; Xue, Y.; Peng, J. (2021). [Comprehensive evaluation of satellite-based and reanalysis soil moisture products using in situ observations over China](https://doi.org/10.5194/hess-25-4209-2021). Hydrology and Earth System Sciences, 25(7), 4209-4229. doi:10.5194/hess-25-4209-2021. 

* Mizukami, N.; Rakovec, O.; Newman, A.J.; Clark, M.P.; Wood, A.W.; Gupta, H.V.; Kumar, R.: (2019). [On the choice of calibration metrics for "high-flow" estimation using hydrologic models](https://doi.org/10.5194/hess-23-2601-2019), Hydrology Earth System Sciences 23, 2601-2614, doi:10.5194/hess-23-2601-2019. 

* Moriasi, D.N.; Arnold, J.G.; van Liew, M.W.; Bingner, R.L.; Harmel, R.D.; Veith, T.L. (2007). [Model evaluation guidelines for systematic quantification of accuracy in watershed simulations](https://swat.tamu.edu/media/1312/moriasimodeleval.pdf). Transactions of the ASABE. 50(3):885-900.

* Nash, J.E. and Sutcliffe, J.V. (1970). [River flow forecasting through conceptual models. Part 1: a discussion of principles](https://doi.org/10.1016/0022-1694(70)90255-6), Journal of Hydrology 10, pp. 282-290. doi:10.1016/0022-1694(70)90255-6. 

* Pearson, K. (1920). [Notes on the history of correlation](https://doi.org/10.2307/2331722). Biometrika, 13(1), 25-45. doi:10.2307/2331722. 

* Pfannerstill, M.; Guse, B.; Fohrer, N. (2014). [Smart low flow signature metrics for an improved overall performance evaluation of hydrological models](https://doi.org/10.1016/j.jhydrol.2013.12.044). Journal of Hydrology, 510, 447-458. doi:10.1016/j.jhydrol.2013.12.044.

* Pizarro, A.; Jorquera, J. (2024). [Advancing objective functions in hydrological modelling: Integrating knowable moments for improved simulation accuracy](https://doi.org/10.1016/j.jhydrol.2024.131071). Journal of Hydrology, 634, 131071. doi:10.1016/j.jhydrol.2024.131071. 

* Pool, S.; Vis, M.; Seibert, J. (2018). [Evaluating model performance: towards a non-parametric variant of the Kling-Gupta efficiency](https://doi.org/10.1080/02626667.2018.1552002). Hydrological Sciences Journal, 63(13-14), pp.1941-1953. doi:10.1080/02626667.2018.1552002. 

* Pushpalatha, R.; Perrin, C.; Le Moine, N.; Andreassian, V. (2012). [A review of efficiency criteria suitable for evaluating low-flow simulations](https://doi.org/10.1016/j.jhydrol.2011.11.055). Journal of Hydrology, 420, 171-182. doi:10.1016/j.jhydrol.2011.11.055.

* Santos, L.; Thirel, G.; Perrin, C. (2018). [Pitfalls in using log-transformed flows within the KGE criterion](https://doi.org/10.5194/hess-22-4583-2018). doi:10.5194/hess-22-4583-2018. 

* Schaefli, B., Gupta, H. (2007). [Do Nash values have value?](https://doi.org/10.1002/hyp.6825). Hydrological Processes 21, 2075-2080. doi:10.1002/hyp.6825. 

* Schober, P.; Boer, C.; Schwarte, L.A. (2018). [Correlation coefficients: appropriate use and interpretation](https://doi.org/10.1213/ANE.0000000000002864). Anesthesia and Analgesia, 126(5), 1763-1768. doi:10.1213/ANE.0000000000002864. 

* Schuol, J.; Abbaspour, K.C.; Srinivasan, R.; Yang, H. (2008b), [Estimation of freshwater availability in the West African sub-continent using the SWAT hydrologic model](https://doi.org/10.1016/j.jhydrol.2007.12.025), Journal of Hydrology, 352(1-2), 30, doi:10.1016/j.jhydrol.2007.12.025.

* Sorooshian, S., Q. Duan, and V. K. Gupta. (1993). [Calibration of rainfall-runoff models: Application of global optimization to the Sacramento Soil Moisture Accounting Model](https://doi.org/10.1029/92WR02617), Water Resources Research, 29 (4), 1185-1194, doi:10.1029/92WR02617.

* Spearman, C. (1961). [The Proof and Measurement of Association Between Two Things](https://doi.org/10.1037/11491-005). In J. J. Jenkins and D. G. Paterson (Eds.), Studies in individual differences: The search for intelligence (pp. 45-58). Appleton-Century-Crofts. doi:10.1037/11491-005.

* Tang, G.; Clark, M.P.; Papalexiou, S.M. (2021). [SC-earth: a station-based serially complete earth dataset from 1950 to 2019](https://doi.org/10.1175/JCLI-D-21-0067.1). Journal of Climate, 34(16), 6493-6511. doi:10.1175/JCLI-D-21-0067.1. 

* Yapo P.O.; Gupta H.V.; Sorooshian S. (1996). [Automatic calibration of conceptual rainfall-runoff models: sensitivity to calibration data](https://doi.org/10.1016/0022-1694(95)02918-4). Journal of Hydrology. v181 i1-4. 23-48. doi:10.1016/0022-1694(95)02918-4.

* Yilmaz, K.K., Gupta, H.V. ; Wagener, T. (2008), [A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model](https://doi.org/10.1029/2007WR006716), Water Resources Research, 44, W09417, doi:10.1029/2007WR006716. 

* Willmott, C.J. (1981). [On the validation of models](https://doi.org/10.1080/02723646.1981.10642213). Physical Geography, 2, 184--194. doi:10.1080/02723646.1981.10642213. 

* Willmott, C.J. (1984). [On the evaluation of model performance in physical geography](https://doi.org/10.1007/978-94-017-3048-8_23). Spatial Statistics and Models, G. L. Gaile and C. J. Willmott, eds., 443-460. doi:10.1007/978-94-017-3048-8_23. 

* Willmott, C.J.; Ackleson, S.G. Davis, R.E.; Feddema, J.J.; Klink, K.M.; Legates, D.R.; O'Donnell, J.; Rowe, C.M. (1985), [Statistics for the Evaluation and Comparison of Models](https://doi.org/10.1029/JC090iC05p08995), J. Geophys. Res., 90(C5), 8995-9005. doi:10.1029/JC090iC05p08995. 

* Willmott, C.J.; Matsuura, K. (2005). [(https://doi.org/)Advantages of the mean absolute error (MAE) over the root mean square error (RMSE) in assessing average model performance](https://doi.org/10.3354/cr030079), Climate Research, 30, 79-82, doi:10.3354/cr030079. 

* Willmott, C.J.; Matsuura, K.; Robeson, S.M. (2009). [Ambiguities inherent in sums-of-squares-based error statistics](https://doi.org/10.1016/j.atmosenv.2008.10.005), Atmospheric Environment, 43, 749-752, doi:10.1016/j.atmosenv.2008.10.005. 

* Willmott, C.J.; Robeson, S.M.; Matsuura, K. (2012). [A refined index of model performance](https://doi.org/10.1002/joc.2419). International Journal of climatology, 32(13), pp.2088-2094. doi:10.1002/joc.2419. 

* Willmott, C.J.; Robeson, S.M.; Matsuura, K.; Ficklin, D.L. (2015). [Assessment of three dimensionless measures of model performance](https://doi.org/10.1016/j.envsoft.2015.08.012). Environmental Modelling & Software, 73, pp.167-174. doi:10.1016/j.envsoft.2015.08.012.

* Zambrano-Bigiarini, M.; Bellin, A. (2012). [Comparing goodness-of-fit measures for calibration of models focused on extreme events](http://www.slideshare.net/hzambran/egu2012-11549go-fsforextremeevents4web). EGU General Assembly 2012, Vienna, Austria, 22-27 Apr 2012, EGU2012-11549-1. 


## Vignette 
[Here](https://cran.r-project.org/package=hydroGOF/vignettes/hydroGOF_Vignette.pdf) you can find an introductory vignette illustrating the use of several hydroGOF functions.


## Related Material 

* *R: a statistical environment for hydrological analysis* (**EGU-2010**)  [abstract](http://meetingorganizer.copernicus.org/EGU2010/EGU2010-13008.pdf), [poster](http://www.slideshare.net/hzambran/egu2010-ra-statisticalenvironmentfordoinghydrologicalanalysis-9095709).

* *Comparing Goodness-of-fit Measures for Calibration of Models Focused on Extreme Events* (**EGU-2012**) [abstract](http://meetingorganizer.copernicus.org/EGU2012/EGU2012-11549-1.pdf), [poster](http://www.slideshare.net/hzambran/egu2012-11549go-fsforextremeevents4web).

* *Using R for analysing spatio-temporal datasets: a satellite-based precipitation case study* (**EGU-2017**) [abstract](http://meetingorganizer.copernicus.org/EGU2017/EGU2017-18343.pdf), [poster](https://doi.org/10.5281/zenodo.570145).



## See Also 

* [hydroTSM: Time series management, analysis and interpolation for hydrological modelling](https://cran.r-project.org/package=hydroTSM).

* [hydroPSO: Model-independent Particle Swarm Optimisation (PSO) for environmental/hydrological models](https://cran.r-project.org/package=hydroPSO).

* [RFmerge: Merging of Satellite Datasets with Ground Observations using Random Forests](https://cran.r-project.org/package=RFmerge).
