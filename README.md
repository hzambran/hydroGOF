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

> Zambrano-Bigiarini, Mauricio (2024). hydroGOF: Goodness-of-fit functions for comparison of simulated and observed hydrological time series. R package version 0.5-4. URL:https://cran.r-project.org/package=hydroGOF. doi:10.5281/zenodo.839854.


A BibTeX entry for LaTeX users is

>  @Manual{hydroGOF,  
>    title = {hydroGOF: Goodness-of-fit functions for comparison of simulated and observed hydrological time series},  
>    author = {Zambrano-Bigiarini, Mauricio},  
>    note = {R package version 0.5-4},  
>    year = {2024},
>    url = {https://cran.r-project.org/package=hydroGOF},  
>    doi = {doi:10.5281/zenodo.839854},  
>  }


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
