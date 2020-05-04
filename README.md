<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/triplot)](https://cran.r-project.org/package=triplot)
[![R build status](https://github.com/ModelOriented/triplot/workflows/R-CMD-check/badge.svg)](https://github.com/ModelOriented/triplot/actions?query=workflow%3AR-CMD-check)
[![Codecov test coverage](https://codecov.io/gh/ModelOriented/triplot/branch/master/graph/badge.svg)](https://codecov.io/gh/ModelOriented/triplot?branch=master)
<!-- badges: end -->

  
# triplot

## Overview

The `triplot` package provides an instance-level explainer for the groups of explanatory variables called aspect importance. 

Package enables grouping predictors into entities called aspects. Afterwards, it calculates the contribution of those aspects to the prediction for a given observation.

Furthermore, package delivers functionality called `triplot`. It illustrates how the importance of aspects change depending on the size of aspects.

Key functions: 

* `aspect_importance()` for calculating the feature groups importance (called aspects importance) for a selected observation, 
* `triplot()` for summary of automatic aspect importance grouping,
* `group_variables()` for grouping numeric features into aspects,
* generic `plot()` for better usability of explainer.

The `triplot` package is a part of [DrWhy.AI](http://DrWhy.AI) universe. 


## Installation

```r
devtools::install_github("ModelOriented/triplot")
```
## Demo

To illustrate how the function works, we use titanic example. We build random forest model, group features into aspects and choose new observation to be explained. Then we build DALEX explainer and use it to call aspect importance function. Finally, we print and plot function results. We can observe that personal (age and gender) variables have the biggest contribution to the prediction. This contribution is of a positive type.



## Acknowledgments

Work on this package was financially supported by the 
