## ----loadLibs, include = FALSE------------------------------------------------
library(RecordTest)
library(ggpubr)
data("TX_Zaragoza")
data("ZaragozaSeries")
data("Olympic_records_200m")
library(knitr)
knitr::opts_chunk$set(
  cache = TRUE,
  comment = "#>",
  collapse = TRUE,
  digits = 5,
  tidy = FALSE,
  background = "#FFFF00",
  fig.align = 'center',
  warning = FALSE,
  message = FALSE
  )
RNGkind(sample.kind = 'Rounding')
options(width = 80, digits = 5)
theme_set(theme_bw())

## ----install, eval = FALSE----------------------------------------------------
#  install.packages("RecordTest")

## ----help, eval = FALSE-------------------------------------------------------
#  help("RecordTest-package")

## ----Olympic data-------------------------------------------------------------
library(RecordTest)
library(ggpubr) # To join plots
data(Olympic_records_200m)

or200m <- series_record(L_lower = Olympic_records_200m$time, 
                        R_lower = Olympic_records_200m$value,
                        Trows = 27)

## -----------------------------------------------------------------------------
records(or200m, alpha = c(1,0,1,0)) + ggplot2::ylab("seconds")

## -----------------------------------------------------------------------------
N.plot(or200m, record = c(0,1,0,0))

## -----------------------------------------------------------------------------
N.test(or200m, record = "lower", distribution = "poisson-binomial")

## ----records------------------------------------------------------------------
data(TX_Zaragoza)
records(TX_Zaragoza$TX, alpha = c(1, 1, 1, 0.05))

## ----pre-process--------------------------------------------------------------
TxZ365 <- series_split(TX_Zaragoza$TX, Mcols = 365)
TxZ <- series_uncor(TxZ365)
dim(TxZ)

## -----------------------------------------------------------------------------
series_ties(TxZ365)

## -----------------------------------------------------------------------------
set.seed(23)
TxZ <- series_untie(TxZ)

## -----------------------------------------------------------------------------
L.plot(TxZ365)

## -----------------------------------------------------------------------------
ggpubr::ggarrange(N.plot(TxZ), N.plot(TxZ, weights = function(t) t-1),
        ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## -----------------------------------------------------------------------------
foster.plot(TxZ) + ggplot2::ylim(-2.5, 2.5)

## -----------------------------------------------------------------------------
foster.plot(TxZ, weights = function(t) t-1) + 
  ggplot2::ylim(-85, 85) +
  ggplot2::geom_vline(xintercept = 45, linetype = "dashed")

## -----------------------------------------------------------------------------
foster.test(TxZ, distribution = "normal", weights = function(t) t-1)
foster.test(TxZ, distribution = "t", weights = function(t) t-1)

## -----------------------------------------------------------------------------
ggpubr::ggarrange(p.plot(TxZ, record = c(1,1,0,0)) + 
            ggplot2::ylim(0, 6),
          p.plot(TxZ, record = c(0,0,1,1)) +
            ggplot2::ylim(0, 6),
          ncol = 2, nrow = 1)

## -----------------------------------------------------------------------------
p.regression.test(TxZ, record = 'upper')
p.regression.test(TxZ, record = 'lower')
p.regression.test(series_rev(TxZ), record = 'upper')
p.regression.test(series_rev(TxZ), record = 'lower')

## -----------------------------------------------------------------------------
set.seed(23)
global.test(TxZ, FUN = p.regression.test, B = 1000)

## -----------------------------------------------------------------------------
brown.method(TxZ, weights = function(t) t-1)
N.test(TxZ, weights = function(t) t-1)

## -----------------------------------------------------------------------------
set.seed(23)
p.chisq.test(TxZ, simulate.p.value = TRUE)
lr.test(TxZ, simulate.p.value = TRUE, B = 10000)
score.test(TxZ)

## ---- warning=FALSE-----------------------------------------------------------
ggpubr::ggarrange(
  p.plot(TxZ, plot = 1, record = c(1,1,0,0), 
         smooth.method = stats::loess, span = 0.25),
  p.plot(TxZ, plot = 1, record = c(1,1,0,0), 
         smooth.formula = y ~ I(x-1) - 1 + offset(rep(1, length(x)))),
  p.plot(TxZ, plot = 2, record = c(1,1,0,0)),
  p.plot(TxZ, plot = 3, record = c(1,1,0,0)),
  ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

