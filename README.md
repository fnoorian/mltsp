 Tools for machine-learning based time-series prediction
========================================================

This R package provides tools for machine-learning based time-series prediction.
This includes preprocessing, creating matrices of lags, building a pipeline, and
specialized cross-validation.

### Installation

You can install the latest version from Github:
```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("fnoorian/mltsp")
```

## Usage

This package include two vignettes:

 * [NARX model guide](inst/doc/narx_guide.html): Simple solution for non-linear auto-regressive forecasting.
 * [Pipeline model guide](inst/doc/pipeline_guide.html): Introduces pipeline model and cross-validation tools.

## NARX model example

To build a non-linear auto-regressive model for 
`lh` (Luteinizing Hormone in Blood Samples, `datasets` package) with

 * differencing order `d = 0`,
 * auto-regressive order `p = 3`,
 * seasonal frequency `freq = 1`,
 * seasonal differencing order ` D =0`,
 * seasonal order `P = 0`,
 * `svm` learner from package [e1071](https://cran.r-project.org/package=e1071),
 * No external regressor

use:
```R
library("mltsp")
library("e1071")

spec = build_narx(svm, p = 3, d = 0, P = 0, D = 0, freq = 1)
model = narx(spec, lh, xreg = NULL)
```

These steps can be combined into one (and default values can used):
```R
model = narx(lh, svm, p = 3)
```

To predict 5 steps ahead, simply use:
```R
forecast(model, h = 5)
```

Output is compatible with package [forecast](https://cran.r-project.org/package=forecast).

### Pipeline model example

To build a pipeline for predicting
`lh` (Luteinizing Hormone in Blood Samples from `datasets` package) with
 
 * Preprocessing: differencing with order automatically determined
 * Features: three lags of input data
 * Learner: a linear model
 
use:

```R
library("mltsp")

pp <- list(list("diff", "auto"))
fx <- function(x) cbind(x, lag_windows(x, p = 3))
ln <- SimpleLM
fcster <- mltsp_forecaster(pp, fx, ln)
```

and then apply function `fcster` to data:

```R
fcster(lh, h = 5)
```

### Cross validation example

To get cross-validation score for `fcster` function for a horizon of 5 steps,
with training on at least 10 data points, use:

```R
ts_crossval(lh, fcster, horizon = 5, initial_window = 10)
```

## Contact Information
 * Farzad Noorian <farzad.noorian@gmail.com>
 * Richard Davis <davisconsulting@gmail.com>

## License
All files in this package, including the documentation and vignettes,
are distributed under GNU GPL v2.0 or later license.
For full terms of this license visit <https://www.gnu.org/licenses/gpl-2.0.html>.

Some functionality have been taken from GPL package [caret](https://cran.r-project.org/package=caret).
For a list of other functions dynamicly imported from other packages, see `NAMESPACE` file.

