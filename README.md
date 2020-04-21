<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/triplot)](https://cran.r-project.org/package=triplot)
[![R build status](https://github.com/ModelOriented/triplot/workflows/R-CMD-check/badge.svg)](https://github.com/ModelOriented/triplot/actions?query=workflow%3AR-CMD-check)
[![Codecov test coverage](https://codecov.io/gh/ModelOriented/triplot/branch/master/graph/badge.svg)](https://codecov.io/gh/ModelOriented/triplot?branch=master)
<!-- badges: end -->

  
# triplot

## Overview

The `triplot` package provides instance-level explainer for the groups of explanatory variables. It enables grouping predictors into entities called aspects. Afterwards, it can calculate the contribution of those aspects to the prediction.

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

## Acknowledgments

Work on this package was financially supported by the 
