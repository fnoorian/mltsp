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

This package includes two vignettes:

 * [NARX model guide](https://fnoorian.github.io/mltsp/inst/doc/narx_guide.html): Simple solution for non-linear auto-regressive forecasting.
 * [Pipeline model guide](https://fnoorian.github.io/mltsp/inst/doc/pipeline_guide.html): Introduces pipeline model and cross-validation tools.

### NARX model example

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

## License and contact information

Copyright 2016 Farzad Noorian <farzad.noorian@gmail.com>, Richard Davis <davisconsulting@gmail.com>,
 Anthony Mihirana de Silva <mihids@gmail.com>, Lei Li <leili8878@gmail.com>.

Some functionality have been taken from GPL package [caret](https://cran.r-project.org/package=caret).
For a list of other functions dynamically imported from other R packages, see `NAMESPACE` file.

This package is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This package is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this package.  If not, see <http://www.gnu.org/licenses/>.
