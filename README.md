### Installation

Download and install Rtools: https://cran.r-project.org/bin/windows/Rtools/ *(Rtools required to install packages from GitHub)*

```
# install.packages("devtools")
library(devtools)

install_github("johnplloyd/R_packages/lloydUtils")
library(lloydUtils)

install_github("johnplloyd/R_packages/lloydPerf")
library(lloydPerf)
```

### lloydUtils

Set of general, custom R functions John often uses while coding in R.

Functions included:
```
corner
intersect.iterate
ordered_dot_plot
p_round.display
remove_outliers
transparent_color
write_df
```

### lloydPerf

Set of functions for calculating performance metrics in R.

Functions included:
```
calc_AUCROC (requires package: ROCR)
calc_kappa
calc_MCC
calc_median_error
calc_MSE
calc_RSS
performance.regression
```

### lloydML

Set of functions for calculating performing machine learning tasks in R. Although many ML packages and frameworks exist, I typically do not find they implement their learning in the way that I'd like, and have resorted to building my custom package. For example, this set of functions includes implementation of multiple rounds of cross-validation or bagging, with final prediction scores reported as the average of the multiple rounds.

Functions included:
```
assign_CV_folds
glmnet_feature_weights
iterate_search_lambda
multi_y.consolidate_performances
multi_y.prediction_and_performance_with_validation_set.wrapper
sort_glmnet_weights
train_and_apply_glmnet_model
train_and_apply_model
train_and_apply_under_CV
train_and_apply_with_validation_set
```
