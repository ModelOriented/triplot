# Aspect Importance

## Overview

The `aspectImportance` package provides instance-level explainer for the groups of explanatory variables. It enables grouping predictors into entities called aspects. Afterwards, it can calculate the contribution of those aspects to the prediction.

Key functions: 

* `aspect_importance()` for calculating the feature groups importance (called aspects importance) for a selected observation, 
* `triplot()` for summary of automatic aspect importance grouping,
* `group_variables()` for grouping numeric features into aspects,
* generic `plot()` for better usability of explainer.

The `aspectImportance` package is a part of [DrWhy.AI](http://DrWhy.AI) universe. 


## Installation

```{r}
# the easiest way to get aspectImportance is to install it from CRAN:
install.packages("aspectImportance")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github(".../ingredients")
```

## Acknowledgments

Work on this package was financially supported by the 
