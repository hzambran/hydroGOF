# hydroGOF
[![Research software impact](http://depsy.org/api/package/cran/hydroGOF/badge.svg)](http://depsy.org/package/r/hydroGOF) [![Build Status](https://travis-ci.org/hzambran/hydroGOF.svg?branch=master)](https://travis-ci.org/hzambran/hydroGOF)
hydroGOF is an R package that provides S3 functions implementing both statistical and graphical goodness-of-fit measures between observed and simulated values, mainly oriented to be used during the calibration, validation, and application of hydrological models.
Missing values in observed and/or simulated values can removed before the computations.
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

>  Mauricio Zambrano-Bigiarini. hydroGOF: Goodness-of-fit functions for comparison of simulated and observed hydrological time series. R package version 0.3-10. URL https://github.com/hzambran/hydroGOF. DOI:10.5281/zenodo.840087


A BibTeX entry for LaTeX users is

>  @Manual{hydroGOF,  
>    title = {hydroGOF: Goodness-of-fit functions for comparison of simulated and observed hydrological time series},  
>    author = {{Mauricio Zambrano-Bigiarini}},  
>    note = {R package version 0.3-10},  
>    url = {https://github.com/hzambran/hydroGOF},  
>    doi = {DOI:10.5281/zenodo.840087},  
>  }


## Related Material 

* *R: a statistical environment for hydrological analysis* (**EGU-2010**)  [abstract](http://meetingorganizer.copernicus.org/EGU2010/EGU2010-13008.pdf), [poster](http://www.slideshare.net/hzambran/egu2010-ra-statisticalenvironmentfordoinghydrologicalanalysis-9095709).

* *Comparing Goodness-of-fit Measures for Calibration of Models Focused on Extreme Events* (**EGU-2012**) [abstract](http://meetingorganizer.copernicus.org/EGU2012/EGU2012-11549-1.pdf), [poster](http://www.slideshare.net/hzambran/egu2012-11549go-fsforextremeevents4web).

* *Using R for analysing spatio-temporal datasets: a satellite-based precipitation case study* (**EGU-2017**) [abstract](http://meetingorganizer.copernicus.org/EGU2017/EGU2017-18343.pdf), [poster](https://doi.org/10.5281/zenodo.570145).



## See Also 

* [hydroTSM: Time series management, analysis and interpolation for hydrological modelling](https://github.com/hzambran/hydroTSM).

* [hydroPSO: Model-independent Particle Swarm Optimisation (PSO) for environmental/hydrological models](https://github.com/hzambran/hydroPSO).


