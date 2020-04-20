<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/triplot)](https://cran.r-project.org/package=triplot)
[![Travis build status](https://travis-ci.com/kasiapekala/triplot.svg?branch=master)](https://travis-ci.com/kasiapekala/triplot)
[![Codecov test coverage](https://codecov.io/gh/kasiapekala/triplot/branch/master/graph/badge.svg)](https://codecov.io/gh/kasiapekala/triplot?branch=master)
<!-- badges: end -->

  
# Aspect Importance

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
# the easiest way to get triplot is to install it from CRAN:
install.packages("triplot")

# Or the the development version from GitHub:
devtools::install_github(".../triplot")
```

## Acknowledgments

Work on this package was financially supported by the 
