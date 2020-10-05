RecordTest <img src="man/figures/logoRecordTest.png" width="175px" align="right" />
======================

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RecordTest)](https://CRAN.R-project.org/package=RecordTest)
[![cran_checks](https://cranchecks.info/badges/worst/RecordTest)](https://cran.r-project.org/web/checks/check_results_RecordTest.html)
[![Downloads](http://cranlogs.r-pkg.org/badges/RecordTest)](https://CRAN.R-project.org/package=RecordTest)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/RecordTest?color=red)](https://CRAN.R-project.org/package=RecordTest)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
  
The R package *RecordTest* provides inference and descriptive tools based on theory of record to detect trends in time series and describe the record occurrence.  
  
## Installation
You can install the **stable** version from
[CRAN](https://CRAN.R-project.org/package=RecordTest).

```s
install.packages("RecordTest")
```

You can install the **development** version from
[GitHub](https://github.com/JorgeCastilloMateo/RecordTest)

```s
# install.packages("remotes")
remotes::install_github("JorgeCastilloMateo/RecordTest")
```

or with the following arguments to install the vignettes (R >= 3.6 required)

```s
remotes::install_github("JorgeCastilloMateo/RecordTest", build_vignettes = TRUE, dependencies = TRUE)
```

## How to start?
Get started in *RecordTest* with the vignettes

```s
vignette("RecordTest")
```